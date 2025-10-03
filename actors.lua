-- actors.lua
-- collisions, LOS, A*, doors usage, enemies, projectiles

-- collisions
function resolve_circle(x,y,r)
  local px,py=x,y
  local ix,iy=flr(px)+1, flr(py)+1
  for j=iy-1,iy+1 do
    for i=ix-1,ix+1 do
      if solid_cell(i,j) then
        local minx,maxx=i-1,i
        local miny,maxy=j-1,j
        local qx=mid(minx, px, maxx)
        local qy=mid(miny, py, maxy)
        local dx=px-qx local dy=py-qy
        local d2=dx*dx+dy*dy
        if d2 < r*r-0.0001 then
          if dx==0 and dy==0 then
            local left=px-minx local right=maxx-px
            local top=py-miny  local bottom=maxy-py
            local ox=min(left,right) local oy=min(top,bottom)
            if ox<oy then px += (left<right) and (-(r-left)) or (r-right)
            else py += (top<bottom) and (-(r-top)) or (r-bottom) end
          else
            local d=sqrt(d2) local push=(r - d)/max(0.0001,d)
            px += dx*push py += dy*push
          end
        end
      end
    end
  end
  return px,py
end

function move_circle(x,y,dx,dy,r)
  local m=max(abs(dx),abs(dy)) local steps=max(1,flr(m/0.05)+1)
  dx/=steps dy/=steps
  for i=1,steps do x+=dx y+=dy x,y=resolve_circle(x,y,r) end
  local ix,iy=flr(x)+1,flr(y)+1
  if solid_cell(ix,iy) then
    for rr=1,6 do
      for dy=-rr,rr do for dx=-rr,rr do
        local nx,ny=ix+dx,iy+dy
        if nx>=1 and ny>=1 and nx<=W and ny<=H and not solid_cell(nx,ny) then
          return cell_center_world(nx),cell_center_world(ny)
        end
      end end
    end
  end
  return x,y
end

-- LOS via DDA (transparent if door is fully open)
function los_dda(ax,ay,bx,by,limit)
  local rdx, rdy = bx-ax, by-ay
  local len=max(0.0001,sqrt(rdx*rdx+rdy*rdy))
  rdx/=len rdy/=len
  local mapx,mapy=flr(ax),flr(ay)
  local ddx=(rdx==0) and BIG or abs(1/rdx)
  local ddy=(rdy==0) and BIG or abs(1/rdy)
  local stepx,stepy,sdx,sdy
  if rdx<0 then stepx=-1 sdx=(ax-mapx)*ddx else stepx=1 sdx=(mapx+1-ax)*ddx end
  if rdy<0 then stepy=-1 sdy=(ay-mapy)*ddy else stepy=1 sdy=(mapy+1-ay)*ddy end
  local guard=0 local lim=limit or 256
  while guard<lim do
    guard+=1
    if sdx<sdy then sdx+=ddx mapx+=stepx else sdy+=ddy mapy+=stepy end
    local ix,iy=mapx+1,mapy+1
    local t=cell(ix,iy)
    if t>0 then
      if t==DOOR_CODE or t==LOCKED_DOOR_CODE then
        local d=door_at(ix,iy)
        if not (d and d.anim>=1) then return false end
      else
        return false
      end
    end
    if mapx==flr(bx) and mapy==flr(by) then return true end
  end
  return true
end

-- doors
function open_door_tile(ix,iy,allow_locked)
  local d=door_at(ix,iy)
  if not d or d.removed then return false end
  if d.locked and not allow_locked then return false end
  d.open=true d.timer=DOOR_OPEN_TIME
  return true
end

function try_open_door_at(x,y,require_key)
  local best_i=-1 local best_d=30000
  for i=1,#doors do
    local d=doors[i]
    if not d.removed and (not d.open or d.anim<1) then
      local wx,wy=cell_center_world(d.x),cell_center_world(d.y)
      local dd=dist(wx,wy,x,y)
      if dd<USE_RADIUS and dd<best_d then best_d=dd best_i=i end
    end
  end
  if best_i==-1 then return false end
  local d=doors[best_i]
  if d.locked and require_key and not player.has_key then return false end
  d.open=true d.timer=DOOR_OPEN_TIME
  return true
end

function update_doors()
  local step=1/DOOR_ANIM_FRAMES
  for d in all(doors) do
    if d.removed then
    elseif d.open then
      if d.anim<1 then d.anim=min(1,d.anim+step) end
      if d.timer>0 then d.timer-=1 end
      if d.timer<=0 then
        local wx,wy=cell_center_world(d.x),cell_center_world(d.y)
        if dist(wx,wy,player.x,player.y)>0.45 then
          local clear=true
          for e in all(enemies) do if dist(wx,wy,e.x,e.y)<0.45 then clear=false break end end
          if clear then d.open=false end
        end
      end
    else
      if d.anim>0 then d.anim=max(0,d.anim-step) end
    end
  end
end

-- projectiles
function spawn_projectile(x,y,tx,ty,from)
  local dx,dy=tx-x,ty-y
  local len=max(0.0001,sqrt(dx*dx+dy*dy)) dx/=len dy/=len
  local sp = (from==1) and 0.22 or 0.12
  local life = (from==1) and 60 or 90
  add(projectiles,{x=x,y=y,dx=dx,dy=dy,spd=sp,life=life,from=from})
end

function move_projectile(pr)
  local tdx,tdy=pr.dx*pr.spd, pr.dy*pr.spd
  local steps=max(1,flr(max(abs(tdx),abs(tdy))/0.04)+1)
  local sdx, sdy = tdx/steps, tdy/steps
  for i=1,steps do
    pr.x+=sdx pr.y+=sdy
    if solid_at_world(pr.x,pr.y) then return true end
  end
  return false
end

function update_projectiles()
  for i=#projectiles,1,-1 do
    local pr=projectiles[i]
    local hit=move_projectile(pr)
    pr.life-=1
    if pr.life<=0 or hit then
      deli(projectiles,i)
    else
      if pr.from==0 then
        if dist(pr.x,pr.y,player.x,player.y)<0.4 then
          if player.hurt_cd<=0 then
            player.hp-=1 player.hurt_cd=30
            if player.hp<=0 then game_state=2 end
          end
          deli(projectiles,i)
        end
      else
        for ei=#enemies,1,-1 do
          local e=enemies[ei]
          if dist(pr.x,pr.y,e.x,e.y) < e.r+0.25 then
            e.hp-=1 deli(projectiles,i) break
          end
        end
      end
    end
  end
end

-- A* (bounded, budgeted)
function astar(sx,sy,tx,ty,rad,max_iter)
  rad=rad or 100
  max_iter=max_iter or 6000
  local minx=max(1,min(sx,tx)-rad)
  local maxx=min(W,max(sx,tx)+rad)
  local miny=max(1,min(sy,ty)-rad)
  local maxy=min(H,max(sy,ty)+rad)
  local w=maxx-minx+1
  local h=maxy-miny+1
  local function inb(x,y) return x>=minx and y>=miny and x<=maxx and y<=maxy end
  if not inb(tx,ty) or not inb(sx,sy) then return nil end
  local function lx(x) return x-minx+1 end
  local function ly(y) return y-miny+1 end

  local g={} local px={} local py={} local closed={} local open={}
  for j=1,h do
    local gr={} local prx={} local pry={} local cl={}
    for i=1,w do gr[i]=30000 prx[i]=0 pry[i]=0 cl[i]=false end
    g[j]=gr px[j]=prx py[j]=pry closed[j]=cl
  end
  local ox,oy=lx(sx),ly(sy)
  local txl,tyl=lx(tx),ly(ty)
  g[oy][ox]=0
  add(open,{x=ox,y=oy,g=0,f=abs(ox-txl)+abs(oy-tyl)})

  local it=0
  while #open>0 and it<max_iter do
    it+=1
    local bi=1
    for i=2,#open do if open[i].f<open[bi].f then bi=i end end
    local cur=open[bi]
    deli(open,bi)
    if not closed[cur.y][cur.x] then
      closed[cur.y][cur.x]=true
      if cur.x==txl and cur.y==tyl then
        local path={}
        local cx,cy=cur.x,cur.y
        while not (cx==ox and cy==oy) do
          add(path,{x=cx+minx-1,y=cy+miny-1},1)
          local nx=px[cy][cx] local ny=py[cy][cx]
          cx,cy=nx,ny
        end
        return path
      end
      local dirs={{1,0},{-1,0},{0,1},{0,-1}}
      for d in all(dirs) do
        local nx,ny=cur.x+d[1], cur.y+d[2]
        if nx>=1 and ny>=1 and nx<=w and ny<=h and not closed[ny][nx] then
          local gx,gy=nx+minx-1, ny+miny-1
          if passable_ai(gx,gy) then
            local tg=cur.g+1
            if tg < g[ny][nx] then
              g[ny][nx]=tg
              px[ny][nx]=cur.x py[ny][nx]=cur.y
              local f=tg + abs(nx-txl)+abs(ny-tyl)
              add(open,{x=nx,y=ny,g=tg,f=f})
            end
          end
        end
      end
    end
  end
  return nil
end

-- enemies & player fire
function player_fire()
  if player.fire_cd>0 then return end
  local dirx,diry=cos(player.a),sin(player.a)
  local sx=player.x+dirx*0.25 local sy=player.y+diry*0.25
  spawn_projectile(sx,sy, sx+dirx, sy+diry, 1)
  player.fire_cd=10
end

local function enemy_try_open_doors(e)
  local best_i=-1 local best_d=30000
  for i=1,#doors do
    local d=doors[i]
    if not d.removed and (not d.locked) and (not d.open or d.anim<1) then
      local wx,wy=cell_center_world(d.x),cell_center_world(d.y)
      local dd=dist(wx,wy,e.x,e.y)
      if dd<1.0 and dd<best_d then best_d=dd best_i=i end
    end
  end
  if best_i~=-1 then local d=doors[best_i] d.open=true d.timer=DOOR_OPEN_TIME end
end

function follow_path(e)
  if not e.path or #e.path==0 or not e.path_i then return false end
  if e.path_i>#e.path then return false end
  local node=e.path[e.path_i]
  local nx,ny=node.x,node.y
  local t=cell(nx,ny)
  if t==DOOR_CODE then
    local wx,wy=cell_center_world(nx),cell_center_world(ny)
    if dist(wx,wy,e.x,e.y)<1.0 then open_door_tile(nx,ny,false) end
  end
  local wx,wy=cell_center_world(nx),cell_center_world(ny)
  local dx,dy=wx-e.x, wy-e.y
  local d=sqrt(dx*dx+dy*dy)
  if d<0.1 then
    e.path_i+=1
    if e.path_i>#e.path then return false end
    node=e.path[e.path_i]
    nx,ny=node.x,node.y
    wx,wy=cell_center_world(nx),cell_center_world(ny)
    dx,dy=wx-e.x, wy-e.y
    d=sqrt(dx*dx+dy*dy)
  end
  if d>0 then
    local ux,uy=dx/d, dy/d
    e.x,e.y=move_circle(e.x,e.y,ux*e.spd,uy*e.spd,e.r)
    return true
  end
  return false
end

function replan_to_player(e)
  if e.repath_cd>0 then e.repath_cd-=1 return end
  if not e.had_los and (not e.path) then return end
  if plan_budget<=0 then return end
  plan_budget-=1
  e.repath_cd=18
  local sx,sy=flr(e.x)+1, flr(e.y)+1
  local tx,ty=flr(player.x)+1, flr(player.y)+1
  local p=astar(sx,sy,tx,ty,100,6000)
  if p then e.path=p e.path_i=1 end
end

function update_enemies()
  for ei=#enemies,1,-1 do
    local e=enemies[ei]
    local dx,dy=player.x-e.x, player.y-e.y
    local d2=dx*dx+dy*dy
    local d=max(0.0001,sqrt(d2))
    local ux,uy=dx/d,dy/d

    if e.los_cd<=0 then
      if d2 < 196 then e.los = los_dda(e.x,e.y,player.x,player.y,256) else e.los=false end
      if e.los then e.had_los=true end
      e.los_cd = e.los and 2 or 4
    else
      e.los_cd -= 1
    end

    enemy_try_open_doors(e)

    if e.t==0 then
      if e.los then
        e.path=nil
        e.x,e.y=move_circle(e.x,e.y,ux*e.spd,uy*e.spd,e.r)
      else
        if e.had_los then if (not follow_path(e)) then replan_to_player(e) end end
      end
      if e.mcd>0 then e.mcd-=1 end
      if d<0.75 and e.mcd<=0 then
        if player.hurt_cd<=0 then
          player.hp-=1 player.hurt_cd=30
          if player.hp<=0 then game_state=2 end
        end
        e.mcd=14
      end
    else
      if e.los then
        e.path=nil
        if d>6 then e.x,e.y=move_circle(e.x,e.y,ux*e.spd,uy*e.spd,e.r)
        elseif d<2 then e.x,e.y=move_circle(e.x,e.y,-ux*e.spd,-uy*e.spd,e.r) end
        if e.cd>0 then e.cd-=1 end
        if d<12 and e.cd<=0 then spawn_projectile(e.x,e.y,player.x,player.y,0) e.cd=52 end
      else
        if e.had_los then if (not follow_path(e)) then replan_to_player(e) end end
      end
    end

    if e.hp<=0 then deli(enemies,ei) end
  end
end
