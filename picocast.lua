-- pico-8 raycaster (doors + los-based AI, exit sprite, idx1 textures)
-- 0 = empty; 1+ = walls; DOOR_CODE=9 (closed door behaves like wall)

scr_w,scr_h=128,128
half_h=64
BIG=32767

-- camera / movement
fov_deg=60
fov_turn=fov_deg/360
move_spd=0.135
rot_spd=2.7/360

-- textures: 32x32 (4x4 tiles)
tex_w,tex_h=32,32
span=tex_w/8          -- 4 tiles per 32px
-- three wall variants start at SPRITE TILE #1, laid horizontally
WALL_SRC0=1
WALL_SRC1=1+span
WALL_SRC2=1+span*2
-- door texture source top-left tile (given)
DOOR_SRC=64
-- exit (billboard sprite top-left tile; change if you store it elsewhere)
EXIT_SPR=96

-- atlas row in MAP to mirror textures for tline()
atlas_ay=0

-- door params
DOOR_CODE=9
DOOR_OPEN_TIME=180  -- frames the door stays open once triggered
DOOR_TRIGGER_DIST=1.0

-- colors
col_sky=12
col_floor=3
col_proj_en=8
col_proj_pl=10
col_hitfx=8

-- world size
W,H=50,50
lvlz=1
level={}
rooms={}
centers={}
doors={} -- {x,y,open,timer}

-- player
p={x=3.5,y=3.5,a=0,hp=3,hurt_cd=0,fire_cd=0}
game_state=0           -- 0=play, 1=win, 2=dead
exit_ix,exit_iy=47,47

-- enemies & projectiles
enemies={}       -- {x,y,t(0 melee/1 ranged), spd, r, cd, hp}
projectiles={}   -- {x,y,dx,dy,spd,life,from(0=en,1=pl)}

-- enemy sprites (32x32 blocks) starting at tile 128
SPR_MELEE=128
SPR_RANGED=128+4

-- ===== utils =====
function irnd(a,b) return flr(rnd(b-a+1))+a end
function clamp(v,a,b) if v<a then return a elseif v>b then return b else return v end end
function cell(x,y) if x<1 or y<1 or x>W or y>H then return 1 end return level[lvlz][y][x] end
function setcell(x,y,v) if x>=1 and y>=1 and x<=W and y<=H then level[lvlz][y][x]=v end end
function cell_center_world(i) return i-0.5 end
function dist(a,b,c,d) return sqrt((a-c)^2+(b-d)^2) end

-- ===== build atlas from arbitrary 32x32 sources =====
-- sources: array of top-left sprite tile indices (e.g. {1,5,9,64})
function build_atlas_from_sources(sources)
  for idx=1,#sources do
    local src= sources[idx]
    local src_c= src%16
    local src_r= src\16
    local ax=(idx-1)*span
    local ay=atlas_ay
    for ty=0,span-1 do
     for tx=0,span-1 do
       local spr=(src_r+ty)*16 + (src_c+tx)
       mset(ax+tx, ay+ty, spr)
     end
    end
  end
end

-- ===== corridor + rooms generation =====
local function overlaps(rx,ry,rw,rh,rooms,pad)
  for r in all(rooms) do
    if not (rx+rw+pad < r.x-pad or r.x+r.w+pad < rx-pad
      or ry+rh+pad < r.y-pad or r.y+r.h+pad < ry-pad) then
      return true
    end
  end
  return false
end

local function carve_rect(rx,ry,rw,rh)
  for y=ry,ry+rh do for x=rx,rx+rw do setcell(x,y,0) end end
end

local function carve_corridor(ax,ay,bx,by)
  if rnd()<0.5 then
    local sx,ex=ax,bx if sx>ex then sx,ex=ex,sx end
    for x=sx,ex do setcell(x,ay,0) end
    local sy,ey=ay,by if sy>ey then sy,ey=ey,sy end
    for y=sy,ey do setcell(bx,y,0) end
  else
    local sy,ey=ay,by if sy>ey then sy,ey=ey,sy end
    for y=sy,ey do setcell(ax,y,0) end
    local sx,ex=ax,bx if sx>ex then sx,ex=ex,sx end
    for x=sx,ex do setcell(x,by,0) end
  end
end

local function manhattan(x1,y1,x2,y2) return abs(x1-x2)+abs(y1-y2) end

local function prim_connect()
  local n=#centers if n<=1 then return end
  local in_tree={} local tree={1} in_tree[1]=true
  while #tree<n do
    local bi,bj,bd=nil,nil,BIG
    for i in all(tree) do
      local a=centers[i]
      for j=1,n do
        if not in_tree[j] then
          local b=centers[j]
          local d=manhattan(a.x,a.y,b.x,b.y)
          if d<bd then bd=d bi=i bj=j end
        end
      end
    end
    local a,b=centers[bi],centers[bj]
    carve_corridor(a.x,a.y,b.x,b.y)
    add(tree,bj) in_tree[bj]=true
  end
end

local function add_extra_links(k)
  for i=1,k do
    local a=centers[irnd(1,#centers)]
    local b=centers[irnd(1,#centers)]
    if a~=b then carve_corridor(a.x,a.y,b.x,b.y) end
  end
end

local function sprinkle_junctions(prob)
  for y=2,H-1 do
    for x=2,W-1 do
      if cell(x,y)==1 and rnd()<prob then
        local lr=(cell(x-1,y)==0 and cell(x+1,y)==0)
        local ud=(cell(x,y-1)==0 and cell(x,y+1)==0)
        if lr or ud then setcell(x,y,0) end
      end
    end
  end
end

local function choose_exit(sx,sy)
  local distv={} for y=1,H do local r={} for x=1,W do r[x]=-1 end distv[y]=r end
  local q={{x=sx,y=sy}} local head=1
  distv[sy][sx]=0
  local fx,fy=sx,sy local best=0
  while head<=#q do
    local c=q[head] head+=1
    local d=distv[c.y][c.x]
    if d>best then best=d fx,fy=c.x,c.y end
    local dirs={{1,0},{-1,0},{0,1},{0,-1}}
    for di in all(dirs) do
      local nx,ny=c.x+di[1],c.y+di[2]
      if nx>=1 and ny>=1 and nx<=W and ny<=H and cell(nx,ny)==0 and distv[ny][nx]<0 then
        distv[ny][nx]=d+1 add(q,{x=nx,y=ny})
      end
    end
  end
  exit_ix,exit_iy=fx,fy
end

-- ----- place doors at room/corridor boundaries -----
function place_doors()
  doors={}
  for rid=1,#rooms do
    local r=rooms[rid]
    -- top/bottom edges
    for x=r.x, r.x+r.w do
      local y=r.y
      if y>1 and cell(x,y)==0 and cell(x,y-1)==0 and rnd()<0.65 then
        if not (x==exit_ix and y==exit_iy) then setcell(x,y,DOOR_CODE) add(doors,{x=x,y=y,open=false,timer=0}) end
      end
      y=r.y+r.h
      if y<H and cell(x,y)==0 and cell(x,y+1)==0 and rnd()<0.65 then
        if not (x==exit_ix and y==exit_iy) then setcell(x,y,DOOR_CODE) add(doors,{x=x,y=y,open=false,timer=0}) end
      end
    end
    -- left/right edges
    for y=r.y, r.y+r.h do
      local x=r.x
      if x>1 and cell(x,y)==0 and cell(x-1,y)==0 and rnd()<0.65 then
        if not (x==exit_ix and y==exit_iy) then setcell(x,y,DOOR_CODE) add(doors,{x=x,y=y,open=false,timer=0}) end
      end
      x=r.x+r.w
      if x<W and cell(x,y)==0 and cell(x+1,y)==0 and rnd()<0.65 then
        if not (x==exit_ix and y==exit_iy) then setcell(x,y,DOOR_CODE) add(doors,{x=x,y=y,open=false,timer=0}) end
      end
    end
  end
  -- de-dup (in case we hit same cell twice)
  local seen={}
  for i=#doors,1,-1 do
    local d=doors[i]
    local key=d.x..","..d.y
    if seen[key] then deli(doors,i) else seen[key]=true end
  end
end

-- open/close doors based on proximity & timer
function update_doors()
  for d in all(doors) do
    local wx,wy=cell_center_world(d.x),cell_center_world(d.y)
    local near=false
    if dist(wx,wy,p.x,p.y)<DOOR_TRIGGER_DIST then near=true end
    if not near then
      for e in all(enemies) do if dist(wx,wy,e.x,e.y)<DOOR_TRIGGER_DIST then near=true break end end
    end

    if not d.open then
      if near then
        d.open=true d.timer=DOOR_OPEN_TIME
        setcell(d.x,d.y,0)
      end
    else
      if d.timer>0 then d.timer-=1 end
      local clear=true
      -- don't close onto player or enemies
      if dist(wx,wy,p.x,p.y) < 0.45 then clear=false end
      if clear then
        for e in all(enemies) do if dist(wx,wy,e.x,e.y) < 0.45 then clear=false break end end
      end
      if d.timer<=0 and clear then
        d.open=false
        setcell(d.x,d.y,DOOR_CODE)
      end
    end
  end
end

-- ===== spawn =====
function build_level()
  -- fill with walls
  local slice={}
  for y=1,H do local row={} for x=1,W do row[x]=1 end slice[y]=row end
  level={[lvlz]=slice}

  -- rooms & centers
  rooms={}
  centers={}
  local max_rooms=18 local tries=0
  while #rooms<max_rooms and tries<500 do
    tries+=1
    local rw=irnd(6,12) local rh=irnd(6,12)
    local rx=irnd(3,W-2-rw) local ry=irnd(3,H-2-rh)
    if not overlaps(rx,ry,rw,rh,rooms,1) then
      carve_rect(rx,ry,rw,rh)
      add(rooms,{x=rx,y=ry,w=rw,h=rh})
      add(centers,{x=rx+rw\2,y=ry+rh\2})
    end
  end
  if #rooms==0 then
    carve_rect(10,10,30,30)
    add(rooms,{x=10,y=10,w=30,h=30})
    add(centers,{x=25,y=25})
  end

  prim_connect()
  add_extra_links(#centers\3)
  sprinkle_junctions(0.06)

  -- start near (3,3): closest center
  local bi=1 local bd=BIG
  for i=1,#centers do
    local c=centers[i] local d=manhattan(c.x,c.y,3,3)
    if d<bd then bd=d bi=i end
  end
  local s=centers[bi]
  p.x=cell_center_world(s.x) p.y=cell_center_world(s.y)
  p.hp=3 p.hurt_cd=0 p.fire_cd=0

  choose_exit(s.x,s.y)
  place_doors()
  spawn_enemies()
end

function random_pos_in_room(r)
  for k=1,60 do
    local x=irnd(r.x+1, r.x+r.w-1)
    local y=irnd(r.y+1, r.y+r.h-1)
    if cell(x,y)==0 then return cell_center_world(x),cell_center_world(y) end
  end
end

function spawn_enemies()
  enemies={} projectiles={}
  for i=1,6 do
    local rid=irnd(1,#rooms)
    local rx,ry=random_pos_in_room(rooms[rid])
    if rx and dist(rx,ry,p.x,p.y)>8 then
      add(enemies,{x=rx,y=ry,t=0,spd=0.045,r=0.18,cd=0,hp=3})
    end
  end
  for i=1,4 do
    local rid=irnd(1,#rooms)
    local rx,ry=random_pos_in_room(rooms[rid])
    if rx and dist(rx,ry,p.x,p.y)>10 then
      add(enemies,{x=rx,y=ry,t=1,spd=0.038,r=0.18,cd=0,hp=2})
    end
  end
end

-- ===== robust circle-vs-grid collision =====
function resolve_circle(x,y,r)
  local px,py=x,y
  local ix,iy=flr(px)+1, flr(py)+1
  for j=iy-1,iy+1 do
    for i=ix-1,ix+1 do
      if cell(i,j)>0 then
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
  if cell(ix,iy)>0 then
    for rr=1,6 do
      for dy=-rr,rr do for dx=-rr,rr do
        local nx,ny=ix+dx,iy+dy
        if nx>=1 and ny>=1 and nx<=W and ny<=H and cell(nx,ny)==0 then
          return cell_center_world(nx),cell_center_world(ny)
        end
      end end
    end
  end
  return x,y
end

-- projectiles: explicit microstep, stop on wall
function move_projectile(pr)
  local tdx=pr.dx*pr.spd
  local tdy=pr.dy*pr.spd
  local steps=max(1,flr(max(abs(tdx),abs(tdy))/0.04)+1)
  local sdx=tdx/steps local sdy=tdy/steps
  for i=1,steps do
    pr.x+=sdx pr.y+=sdy
    if cell(flr(pr.x)+1,flr(pr.y)+1)>0 then return true end
  end
  return false
end

-- ===== LOS (doors block when closed because cell()>0) =====
function has_los(ax,ay,bx,by,limit)
  local dx,dy=bx-ax,by-ay
  local len=max(0.0001,sqrt(dx*dx+dy*dy))
  dx/=len dy/=len
  local x,y=ax,ay
  local steps=min(limit or 80, flr(len*3)+1)
  for i=1,steps do
    x+=dx y+=dy
    if cell(flr(x)+1,flr(y)+1)>0 then return false end
  end
  return true
end

-- ===== enemies & projectiles =====
function spawn_projectile(x,y,tx,ty,from)
  local dx,dy=tx-x,ty-y
  local len=max(0.0001,sqrt(dx*dx+dy*dy)) dx/=len dy/=len
  local sp = (from==1) and 0.22 or 0.12 -- player faster; enemy slower
  local life = (from==1) and 60 or 90
  add(projectiles,{x=x,y=y,dx=dx,dy=dy,spd=sp,life=life,from=from})
end

function player_fire()
  if p.fire_cd>0 then return end
  local dirx,diry=cos(p.a),sin(p.a)
  local sx=p.x+dirx*0.25
  local sy=p.y+diry*0.25
  spawn_projectile(sx,sy, sx+dirx, sy+diry, 1)
  p.fire_cd=10
end

function update_enemies()
  for ei=#enemies,1,-1 do
    local e=enemies[ei]
    local dx,dy=p.x-e.x, p.y-e.y
    local d=max(0.0001,sqrt(dx*dx+dy*dy))
    local ux,uy=dx/d,dy/d

    -- relentless if LOS; otherwise idle (no pathfinding)
    local see = has_los(e.x,e.y,p.x,p.y,120)

    if e.t==0 then
      -- melee
      if see then
        e.x,e.y=move_circle(e.x,e.y,ux*e.spd,uy*e.spd,e.r)
        if d<0.55 and p.hurt_cd<=0 then
          p.hp-=1 p.hurt_cd=30
          if p.hp<=0 then game_state=2 end
        end
      end
    else
      -- ranged
      if see then
        if d>6 then e.x,e.y=move_circle(e.x,e.y,ux*e.spd,uy*e.spd,e.r)
        elseif d<2 then e.x,e.y=move_circle(e.x,e.y,-ux*e.spd,-uy*e.spd,e.r) end
        if e.cd>0 then e.cd-=1 end
        if d<12 and e.cd<=0 then
          spawn_projectile(e.x,e.y,p.x,p.y,0)
          e.cd=52
        end
      end
    end

    -- if killed by player shots
    if e.hp<=0 then deli(enemies,ei) end
  end
end

function update_projectiles()
  for i=#projectiles,1,-1 do
    local pr=projectiles[i]
    local hitwall=move_projectile(pr)
    pr.life-=1
    if pr.life<=0 or hitwall then
      deli(projectiles,i)
    else
      if pr.from==0 then
        if dist(pr.x,pr.y,p.x,p.y)<0.4 then
          if p.hurt_cd<=0 then
            p.hp-=1 p.hurt_cd=30
            if p.hp<=0 then game_state=2 end
          end
          deli(projectiles,i)
        end
      else
        -- hit enemies?
        for ei=#enemies,1,-1 do
          local e=enemies[ei]
          if dist(pr.x,pr.y,e.x,e.y) < e.r+0.25 then
            e.hp-=1
            deli(projectiles,i)
            break
          end
        end
      end
    end
  end
end

-- ===== init & update =====
function _init()
  -- srand(t()) -- uncomment for different layouts
  build_atlas_from_sources({WALL_SRC0,WALL_SRC1,WALL_SRC2,DOOR_SRC})
  build_level()
end

function _update60()
  if game_state~=0 then
    if btnp(4) or btnp(5) then build_level() game_state=0 end
    return
  end

  if p.hurt_cd>0 then p.hurt_cd-=1 end
  if p.fire_cd>0 then p.fire_cd-=1 end

  if btn(0) then p.a-=rot_spd end
  if btn(1) then p.a+=rot_spd end
  if p.a<0 then p.a+=1 end
  if p.a>=1 then p.a-=1 end

  if btnp(4) or btnp(5) then player_fire() end

  local dirx,diry=cos(p.a),sin(p.a)
  local dx,dy=0,0
  if btn(2) then dx+=dirx*move_spd dy+=diry*move_spd end
  if btn(3) then dx-=dirx*move_spd dy-=diry*move_spd end
  if dx~=0 or dy~=0 then p.x,p.y=move_circle(p.x,p.y,dx,dy,0.18) end

  update_doors()
  update_enemies()
  update_projectiles()

  -- win check
  local ex,ey=cell_center_world(exit_ix),cell_center_world(exit_iy)
  if (abs(p.x-ex)<0.4 and abs(p.y-ey)<0.4) then game_state=1 end
end

-- ===== renderer (walls -> zbuf, then exit/enemies/projectiles) =====
zbuf={}
function draw_scene()
  local dirx,diry=cos(p.a),sin(p.a)
  local half_fov=fov_turn/2
  local t_half=sin(half_fov)/max(0.0001,cos(half_fov))
  local planex,planey=-diry*t_half, dirx*t_half

  rectfill(0,0,127,63,col_sky)
  rectfill(0,64,127,127,col_floor)

  local near_clip=0.02

  for x=0,scr_w-1 do
    local camx = x/(scr_w-1)*2-1
    local rdx = dirx + planex*camx
    local rdy = diry + planey*camx

    local mapx,mapy=flr(p.x),flr(p.y)
    local ddx = (rdx==0) and BIG or abs(1/rdx)
    local ddy = (rdy==0) and BIG or abs(1/rdy)

    local stepx,stepy,sdx,sdy
    if rdx<0 then stepx=-1 sdx=(p.x-mapx)*ddx else stepx=1 sdx=(mapx+1-p.x)*ddx end
    if rdy<0 then stepy=-1 sdy=(p.y-mapy)*ddy else stepy=1 sdy=(mapy+1-p.y)*ddy end

    local hit_val=0 local side=0 local guard=0
    while hit_val==0 and guard<512 do
      guard+=1
      if sdx<sdy then sdx+=ddx mapx+=stepx side=0 else sdy+=ddy mapy+=stepy side=1 end
      local t=cell(mapx+1,mapy+1)
      if t>0 then hit_val=t end
    end

    local perp=(side==0) and (sdx-ddx) or (sdy-ddy)
    if perp<near_clip then perp=near_clip end
    zbuf[x]=perp

    if hit_val>0 then
      local line_h=flr(scr_h/perp)
      local draw_start=clamp(half_h - line_h\2,0,scr_h-1)
      local draw_end  =clamp(half_h + line_h\2,0,scr_h-1)

      if draw_end>draw_start then
        local wallx
        if side==0 then
          local dx=(mapx - p.x + (1-stepx)/2)
          local hit_y=p.y + dx*(rdy/rdx)
          wallx=hit_y - flr(hit_y)
        else
          local dy=(mapy - p.y + (1-stepy)/2)
          local hit_x=p.x + dy*(rdx/rdy)
          wallx=hit_x - flr(hit_x)
        end
        local tex_x=flr(wallx*tex_w)
        if (side==0 and rdx>0) or (side==1 and rdy<0) then tex_x=tex_w-1-tex_x end

        -- choose atlas index:
        local atlas_idx
        if hit_val==DOOR_CODE then
          atlas_idx = 3 -- 0-based: after 3 wall variants
        else
          local mxv=max(0,mapx) local myv=max(0,mapy)
          atlas_idx = (side==0) and (mxv%3) or (myv%3) -- 0..2
        end

        local step_px = tex_h/line_h
        local tex_pos = (draw_start - (half_h - line_h/2)) * step_px

        local u=atlas_idx*span + tex_x/8
        local v=atlas_ay + tex_pos/8
        tline(x,draw_start, x,draw_end, u,v, 0, step_px/8)
      end
    end
  end

  -- draw EXIT as billboard (occluded by zbuf)
  draw_exit_billboard(dirx,diry,planex,planey)
  -- enemies & projectiles
  draw_enemies_billboards(dirx,diry,planex,planey)
  draw_projectiles(dirx,diry,planex,planey)
end

-- 32x32 billboard from a 32x32 sprite block top-left tile id
function draw_sprite32_billboard(tile_id, depth, screen_x)
  local s_tx = (tile_id%16)*8
  local s_ty = (tile_id\16)*8
  local src_x,src_y,src_w,src_h = s_tx, s_ty, 32, 32

  local h = flr(scr_h/depth)
  local w = h
  if w<1 or h<1 then return end

  local x0 = flr(screen_x - w/2)
  local y0 = flr(half_h - h/2)
  local x1 = x0 + w - 1
  local y1 = y0 + h - 1
  if x1<0 or x0>127 or y1<0 or y0>127 then return end

  local xs=max(0,x0) local xe=min(127,x1)
  for x=xs,xe do
    if depth < zbuf[x] then
      local u = flr((x - x0) * src_w / w)
      sspr(src_x+u, src_y, 1, src_h, x, y0, 1, h)
    end
  end
end

function draw_exit_billboard(dirx,diry,planex,planey)
  local invdet = 1 / (planex*diry - dirx*planey)
  local ex,ey=cell_center_world(exit_ix),cell_center_world(exit_iy)
  local rx=ex - p.x local ry=ey - p.y
  local tx = invdet * ( diry*rx - dirx*ry)
  local ty = invdet * (-planey*rx + planex*ry)
  if ty>0 then
    local screen_x = flr((scr_w/2)*(1 + tx/ty))
    draw_sprite32_billboard(EXIT_SPR, ty, screen_x)
  end
end

function draw_enemies_billboards(dirx,diry,planex,planey)
  local invdet = 1 / (planex*diry - dirx*planey)
  local order={}
  for i=1,#enemies do
    local e=enemies[i]
    local rx=e.x - p.x
    local ry=e.y - p.y
    local tx = invdet * ( diry*rx - dirx*ry)
    local ty = invdet * (-planey*rx + planex*ry)
    add(order,{i=i,ty=ty})
  end
  for a=1,#order do
    local bi=a
    for b=a+1,#order do if order[b].ty>order[bi].ty then bi=b end end
    if bi~=a then local t=order[a] order[a]=order[bi] order[bi]=t end
  end
  for k=1,#order do
    local e=enemies[order[k].i]
    local rx=e.x - p.x
    local ry=e.y - p.y
    local tx = invdet * ( diry*rx - dirx*ry)
    local ty = invdet * (-planey*rx + planex*ry)
    if ty>0 then
      local screen_x = flr((scr_w/2)*(1 + tx/ty))
      local tile = (e.t==0) and SPR_MELEE or SPR_RANGED
      draw_sprite32_billboard(tile, ty, screen_x)
    end
  end
end

function draw_projectiles(dirx,diry,planex,planey)
  local invdet = 1 / (planex*diry - dirx*planey)
  for pr in all(projectiles) do
    local rx=pr.x - p.x
    local ry=pr.y - p.y
    local tx = invdet * ( diry*rx - dirx*ry)
    local ty = invdet * (-planey*rx + planex*ry)
    if ty>0 then
      local h = flr(scr_h/ty)
      local w = h\2
      local cx = flr((scr_w/2)*(1 + tx/ty))
      local x0 = cx - w\2
      local y0 = half_h - h\2
      local xs=max(0,x0) local xe=min(127,x0+w-1)
      local col = (pr.from==1) and col_proj_pl or col_proj_en
      for x=xs,xe do
        if ty < zbuf[x] then line(x,y0, x, y0+h-1, col) end
      end
    end
  end
end

-- ===== HUD & screens =====
function draw_hud()
  local s="hp: " for i=1,3 do s..=(i<=p.hp and "\143" or "\151") end
  print(s, 2,2, p.hurt_cd>0 and col_hitfx or 7)
  local ex,ey=cell_center_world(exit_ix),cell_center_world(exit_iy)
  local d=dist(p.x,p.y,ex,ey) d=flr(d*10)/10
  print("dist: "..d, 2,10,7)
  print("FPS: "..stat(7), 80,100,7)
  print("CPU: "..stat(1), 80,108,7)
end

function draw_win()
  cls(0) print("\^wyou win!", 38,54,11) print("press ❎/🅾️ to play again", 18,70,6)
end

function draw_dead()
  cls(0) print("\^woops!", 48,50,8) print("you died", 44,62,8) print("press ❎/🅾️ to retry", 22,78,6)
end

function _draw()
  if game_state==1 then draw_win()
  elseif game_state==2 then draw_dead()
  else cls() draw_scene() draw_hud() end
end
