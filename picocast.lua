-- pico-8 raycaster (idx1 walls, 1 door per hall, see-under doors, no 1e9)
-- doors: press 🅾️ near a door to open; closed doors block los/movement.
-- opening doors: background renders through; door top overlays, so you see under it.

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
span=tex_w/8 -- 4 tiles wide

-- wall variants start at sprite tile #1 (sprite #0 is blank)
WALL_SRC0=1
WALL_SRC1=1+span
WALL_SRC2=1+span*2

-- door sprite (32x32 block) top-left tile
DOOR_SRC=64
-- exit (32x32 billboard)
EXIT_SPR=68

-- atlas row for tline sampling
atlas_ay=0

-- door params
DOOR_CODE=9
DOOR_OPEN_TIME=180  -- frames to stay open before closing
USE_RADIUS=1.0
DOOR_ANIM_FRAMES=30

-- colors
col_sky=12
col_floor=3
col_proj_en=8
col_proj_pl=10
col_hitfx=8

-- world
W,H=50,50
lvlz=1
level={}
rooms={}
centers={}
doors={}      -- {x,y,open,anim(0..1),timer}
door_map={}   -- "x,y"->index
player={x=3.5,y=3.5,a=0,hp=3,hurt_cd=0,fire_cd=0}
game_state=0  -- 0=play,1=win,2=dead
exit_ix,exit_iy=47,47

-- enemies & projectiles
enemies={}       -- {x,y,t(0 melee/1 ranged), spd, r, cd, hp, los, los_cd}
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
function dkey(x,y) return x..","..y end
function door_at(ix,iy) local i=door_map[dkey(ix,iy)] if i then return doors[i] end end

-- solid test (doors solid unless fully open)
function solid_cell(ix,iy)
  local t=cell(ix,iy)
  if t==DOOR_CODE then
    local d=door_at(ix,iy)
    if d and d.anim>=1 then return false end
  end
  return t>0
end
function solid_at_world(x,y) return solid_cell(flr(x)+1, flr(y)+1) end

-- ===== atlas from 32x32 sources =====
function build_atlas_from_sources(sources)
  for idx=1,#sources do
    local src=sources[idx]
    local sc, sr = src%16, src\16
    local ax=(idx-1)*span
    for ty=0,span-1 do
      for tx=0,span-1 do
        local spr=(sr+ty)*16+(sc+tx)
        mset(ax+tx, atlas_ay+ty, spr)
      end
    end
  end
end

-- ===== generation: rooms + corridors =====
local function overlaps(rx,ry,rw,rh,rooms,pad)
  for r in all(rooms) do
    if not (rx+rw+pad<r.x-pad or r.x+r.w+pad<rx-pad or ry+rh+pad<r.y-pad or r.y+r.h+pad<ry-pad) then
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

-- corridor helpers
local function corridor_ori(ix,iy)
  local u=cell(ix,iy-1)==0
  local d=cell(ix,iy+1)==0
  local l=cell(ix-1,iy)==0
  local r=cell(ix+1,iy)==0
  if u and d and (not l) and (not r) then return 'v' end
  if l and r and (not u) and (not d) then return 'h' end
  return nil
end

-- place exactly one door per corridor segment (centered)
function place_doors_one_per_corridor()
  doors={} door_map={}
  -- visited grid for corridor cells
  local vis={}
  for y=1,H do local row={} for x=1,W do row[x]=false end vis[y]=row end

  for y=2,H-1 do
    for x=2,W-1 do
      if cell(x,y)==0 and not vis[y][x] then
        local ori=corridor_ori(x,y)
        if ori then
          -- collect contiguous cells along the corridor
          local seg={}
          -- walk negative
          local dx,dy=(ori=='h') and -1 or 0, (ori=='v') and -1 or 0
          local cx,cy=x,y
          while cell(cx,cy)==0 and corridor_ori(cx,cy)==ori and not vis[cy][cx] do
            cx-=dx cy-=dy
          end
          -- step forward once (we walked one too far)
          cx+=dx cy+=dy
          -- now forward positive to collect
          dx,dy=(ori=='h') and 1 or 0, (ori=='v') and 1 or 0
          while cell(cx,cy)==0 and corridor_ori(cx,cy)==ori and not vis[cy][cx] do
            add(seg,{x=cx,y=cy})
            vis[cy][cx]=true
            cx+=dx cy+=dy
          end
          -- choose middle cell for the door (avoid exit cell)
          if #seg>0 then
            local mid=(#seg\2)+1
            local pick=seg[mid]
            if pick.x==exit_ix and pick.y==exit_iy then
              if mid<#seg then pick=seg[mid+1]
              elseif mid>1 then pick=seg[mid-1] end
            end
            -- place door
            if not (pick.x==exit_ix and pick.y==exit_iy) then
              setcell(pick.x,pick.y,DOOR_CODE)
              local d={x=pick.x,y=pick.y,open=false,anim=0,timer=0}
              add(doors,d)
              door_map[dkey(pick.x,pick.y)]=#doors
            end
          end
        else
          vis[y][x]=true -- mark non-straight floors to skip
        end
      end
    end
  end
end

-- ===== spawn/build =====
function random_pos_in_room(r)
  for k=1,60 do
    local x=irnd(r.x+1, r.x+r.w-1)
    local y=irnd(r.y+1, r.y+r.h-1)
    if cell(x,y)==0 then return cell_center_world(x),cell_center_world(y) end
  end
end

function spawn_enemies()
  enemies={}
  for i=1,6 do
    local rid=irnd(1,#rooms)
    local rx,ry=random_pos_in_room(rooms[rid])
    if rx and dist(rx,ry,player.x,player.y)>8 then
      add(enemies,{x=rx,y=ry,t=0,spd=0.045,r=0.18,cd=0,hp=3,los=false,los_cd=0})
    end
  end
  for i=1,4 do
    local rid=irnd(1,#rooms)
    local rx,ry=random_pos_in_room(rooms[rid])
    if rx and dist(rx,ry,player.x,player.y)>10 then
      add(enemies,{x=rx,y=ry,t=1,spd=0.038,r=0.18,cd=0,hp=2,los=false,los_cd=0})
    end
  end
end

function build_level()
  -- fill walls
  local slice={}
  for y=1,H do local row={} for x=1,W do row[x]=1 end slice[y]=row end
  level={[lvlz]=slice}

  rooms={} centers={}
  -- modest room sizes for perf
  local max_rooms=16 local tries=0
  while #rooms<max_rooms and tries<500 do
    tries+=1
    local rw=irnd(5,9) local rh=irnd(5,9)
    local rx=irnd(3,W-2-rw) local ry=irnd(3,H-2-rh)
    if not overlaps(rx,ry,rw,rh,rooms,1) then
      carve_rect(rx,ry,rw,rh)
      add(rooms,{x=rx,y=ry,w=rw,h=rh})
      add(centers,{x=rx+rw\2,y=ry+rh\2})
    end
  end
  if #rooms==0 then
    carve_rect(10,10,24,24)
    add(rooms,{x=10,y=10,w=24,h=24})
    add(centers,{x=22,y=22})
  end

  prim_connect()
  add_extra_links(max(1,#centers\4))
  sprinkle_junctions(0.05)

  -- start near (3,3)
  local bi=1 local bd=BIG
  for i=1,#centers do local c=centers[i] local d=manhattan(c.x,c.y,3,3) if d<bd then bd=d bi=i end end
  local s=centers[bi]
  player.x=cell_center_world(s.x) player.y=cell_center_world(s.y)
  player.hp=3 player.hurt_cd=0 player.fire_cd=0

  choose_exit(s.x,s.y)
  place_doors_one_per_corridor()
  spawn_enemies()
  projectiles={}
end

-- ===== collisions (treat doors via solid_cell) =====
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

-- ===== DDA LOS (open doors transparent) =====
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
      if t==DOOR_CODE then
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

-- ===== doors: use/open/animate/auto-close =====
function use_nearby_door()
  local best_i=-1 local best_d=30000 -- (unused with pico8, we'll just init big)
  for i=1,#doors do
    local d=doors[i]
    if not d.open or d.anim<1 then
      local wx,wy=cell_center_world(d.x),cell_center_world(d.y)
      local dd=dist(wx,wy,player.x,player.y)
      if dd<USE_RADIUS and dd<best_d then best_d=dd best_i=i end
    end
  end
  if best_i==-1 then return end
  local d=doors[best_i]
  d.open=true d.timer=DOOR_OPEN_TIME
end

function update_doors()
  local step=1/DOOR_ANIM_FRAMES
  for d in all(doors) do
    if d.open then
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

-- ===== projectiles =====
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

-- ===== enemies & player fire =====
function player_fire()
  if player.fire_cd>0 then return end
  local dirx,diry=cos(player.a),sin(player.a)
  local sx=player.x+dirx*0.25 local sy=player.y+diry*0.25
  spawn_projectile(sx,sy, sx+dirx, sy+diry, 1)
  player.fire_cd=10
end

function update_enemies()
  for ei=#enemies,1,-1 do
    local e=enemies[ei]
    local dx,dy=player.x-e.x, player.y-e.y
    local d2=dx*dx+dy*dy
    local d=max(0.0001,sqrt(d2))
    local ux,uy=dx/d,dy/d

    if e.los_cd<=0 then
      if d2 < 196 then
        e.los = los_dda(e.x,e.y,player.x,player.y,256)
      else
        e.los = false
      end
      e.los_cd = e.los and 2 or 4
    else
      e.los_cd -= 1
    end

    if e.t==0 then
      if e.los then
        e.x,e.y=move_circle(e.x,e.y,ux*e.spd,uy*e.spd,e.r)
        if d<0.55 and player.hurt_cd<=0 then
          player.hp-=1 player.hurt_cd=30
          if player.hp<=0 then game_state=2 end
        end
      end
    else
      if e.los then
        if d>6 then e.x,e.y=move_circle(e.x,e.y,ux*e.spd,uy*e.spd,e.r)
        elseif d<2 then e.x,e.y=move_circle(e.x,e.y,-ux*e.spd,-uy*e.spd,e.r) end
        if e.cd>0 then e.cd-=1 end
        if d<12 and e.cd<=0 then
          spawn_projectile(e.x,e.y,player.x,player.y,0)
          e.cd=52
        end
      end
    end

    if e.hp<=0 then deli(enemies,ei) end
  end
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

-- ===== init & update =====
function _init()
  -- srand(t()) -- uncomment for new layout each boot
  build_atlas_from_sources({WALL_SRC0,WALL_SRC1,WALL_SRC2,DOOR_SRC})
  build_level()
end

function _update60()
  if game_state~=0 then
    if btnp(4) or btnp(5) then build_level() game_state=0 end
    return
  end

  if player.hurt_cd>0 then player.hurt_cd-=1 end
  if player.fire_cd>0 then player.fire_cd-=1 end

  if btn(0) then player.a-=rot_spd end
  if btn(1) then player.a+=rot_spd end
  if player.a<0 then player.a+=1 end
  if player.a>=1 then player.a-=1 end

  if btnp(5) then player_fire() end      -- ❎ shoot
  if btnp(4) then use_nearby_door() end  -- 🅾️ open/close door

  local dirx,diry=cos(player.a),sin(player.a)
  local dx,dy=0,0
  if btn(2) then dx+=dirx*move_spd dy+=diry*move_spd end
  if btn(3) then dx-=dirx*move_spd dy-=diry*move_spd end
  if dx~=0 or dy~=0 then player.x,player.y=move_circle(player.x,player.y,dx,dy,0.18) end

  update_doors()
  update_enemies()
  update_projectiles()

  local ex,ey=cell_center_world(exit_ix),cell_center_world(exit_iy)
  if abs(player.x-ex)<0.4 and abs(player.y-ey)<0.4 then game_state=1 end
end

-- ===== renderer (see-under doors: background then door overlay) =====
zbuf={}
local door_overlay={} -- per-column overlay info: {perp, tex_x, anim}

function draw_scene()
  local dirx,diry=cos(player.a),sin(player.a)
  local half_fov=fov_turn/2
  local t_half=sin(half_fov)/max(0.0001,cos(half_fov))
  local planex,planey=-diry*t_half, dirx*t_half

  rectfill(0,0,127,63,col_sky)
  rectfill(0,64,127,127,col_floor)

  -- reset overlays
  for i=0,127 do door_overlay[i]=nil end

  local near_clip=0.03

  for x=0,scr_w-1 do
    local camx = x/(scr_w-1)*2-1
    local rdx = dirx + planex*camx
    local rdy = diry + planey*camx

    local mapx,mapy=flr(player.x),flr(player.y)
    local ddx = (rdx==0) and BIG or abs(1/rdx)
    local ddy = (rdy==0) and BIG or abs(1/rdy)
    local stepx,stepy,sdx,sdy
    if rdx<0 then stepx=-1 sdx=(player.x-mapx)*ddx else stepx=1 sdx=(mapx+1-player.x)*ddx end
    if rdy<0 then stepy=-1 sdy=(player.y-mapy)*ddy else stepy=1 sdy=(mapy+1-player.y)*ddy end

    local hit_val=0 local side=0 local guard=0

    while hit_val==0 and guard<256 do
      guard+=1
      if sdx<sdy then sdx+=ddx mapx+=stepx side=0 else sdy+=ddy mapy+=stepy side=1 end
      local ix,iy=mapx+1,mapy+1
      local t=cell(ix,iy)
      if t==DOOR_CODE then
        local d=door_at(ix,iy)
        if d then
          -- compute perp & tex_x for this door step
          local perp=(side==0) and (sdx-ddx) or (sdy-ddy)
          if perp<near_clip then perp=near_clip end
          local wallx
          if side==0 then
            local dx=(mapx - player.x + (1-stepx)/2)
            local hy=player.y + dx*(rdy/(rdx==0 and 0.0001 or rdx))
            wallx=hy - flr(hy)
          else
            local dy=(mapy - player.y + (1-stepy)/2)
            local hx=player.x + dy*(rdx/(rdy==0 and 0.0001 or rdy))
            wallx=hx - flr(hx)
          end
          local tex_x=flr(wallx*tex_w)
          if (side==0 and rdx>0) or (side==1 and rdy<0) then tex_x=tex_w-1-tex_x end
          if tex_x<0 then tex_x=0 elseif tex_x>=tex_w then tex_x=tex_w-1 end

          if d.anim>0 then
            -- see-through: store overlay if nearer than any existing
            local ov=door_overlay[x]
            if (not ov) or (perp<ov.perp) then
              door_overlay[x]={perp=perp, tex_x=tex_x, anim=d.anim}
            end
            -- continue DDA to find real background
          else
            -- closed door: treat as blocking hit
            hit_val=DOOR_CODE
          end
        end
      elseif t>0 then
        hit_val=t
      end
    end

    local perp=(side==0) and (sdx-ddx) or (sdy-ddy)
    if perp<near_clip then perp=near_clip end
    zbuf[x]=perp

    if hit_val>0 then
      local line_h=flr(scr_h/perp)
      local draw_start=half_h - line_h\2
      local draw_end  =draw_start + line_h - 1

      -- texture column via exact intersection
      local wallx
      if side==0 then
        local dx=(mapx - player.x + (1-stepx)/2)
        local hy=player.y + dx*(rdy/(rdx==0 and 0.0001 or rdx))
        wallx=hy - flr(hy)
      else
        local dy=(mapy - player.y + (1-stepy)/2)
        local hx=player.x + dy*(rdx/(rdy==0 and 0.0001 or rdy))
        wallx=hx - flr(hx)
      end
      local tex_x=flr(wallx*tex_w)
      if (side==0 and rdx>0) or (side==1 and rdy<0) then tex_x=tex_w-1-tex_x end
      if tex_x<0 then tex_x=0 elseif tex_x>=tex_w then tex_x=tex_w-1 end

      local tex_sel
      if hit_val==DOOR_CODE then
        -- closed (not opening) doors block fully
        tex_sel=3
      else
        local mxv=max(0,mapx) local myv=max(0,mapy)
        tex_sel = (side==0) and (mxv%3) or (myv%3)
      end

      -- map correct texture window even if column > screen
      local ys = clamp(draw_start,0,scr_h-1)
      local ye = clamp(draw_end,0,scr_h-1)
      if ye>ys then
        local step_px = tex_h/line_h
        local tex_pos = (ys - (half_h - line_h/2)) * step_px
        local u=tex_sel*span + tex_x/8
        local v=atlas_ay + tex_pos/8
        tline(x,ys, x,ye, u,v, 0, step_px/8)
      end
    end
  end

  -- overlay opening doors (draw top part; see under)
  for x=0,127 do
    local ov=door_overlay[x]
    if ov then
      local perp=ov.perp
      local line_h=flr(scr_h/perp)
      local draw_start=half_h - line_h\2
      local vis_h=max(0, line_h - flr(line_h*ov.anim))
      local draw_end=draw_start + vis_h - 1

      local ys = clamp(draw_start,0,scr_h-1)
      local ye = clamp(draw_end,0,scr_h-1)
      if ye>ys and vis_h>0 then
        local step_px = tex_h/line_h
        local tex_pos = (ys - (half_h - line_h/2)) * step_px
        local u=3*span + ov.tex_x/8  -- door in atlas slot #3 (0-based)
        local v=atlas_ay + tex_pos/8
        -- update z for sprite occlusion (coarse per-column)
        if perp<zbuf[x] then zbuf[x]=perp end
        tline(x,ys, x,ye, u,v, 0, step_px/8)
      end
    end
  end

  -- billboards (exit, enemies, projectiles)
  draw_exit_billboard(dirx,diry,planex,planey)
  draw_enemies_billboards(dirx,diry,planex,planey)
  draw_projectiles(dirx,diry,planex,planey)
end

-- 32x32 billboard with per-column z test
function draw_sprite32_billboard(tile_id, depth, screen_x)
  local s_tx=(tile_id%16)*8
  local s_ty=(tile_id\16)*8
  local src_x,src_y,src_w,src_h=s_tx,s_ty,32,32
  local h=flr(scr_h/depth) local w=h
  if w<1 or h<1 then return end
  local x0=flr(screen_x - w/2)
  local y0=flr(half_h - h/2)
  local x1=x0+w-1 if x1<0 or x0>127 then return end
  local xs=max(0,x0) local xe=min(127,x1)
  for x=xs,xe do
    if depth < zbuf[x] then
      local u=flr((x-x0)*src_w/w)
      sspr(src_x+u,src_y,1,src_h, x,y0, 1,h)
    end
  end
end

function draw_exit_billboard(dirx,diry,planex,planey)
  local invdet=1/(planex*diry - dirx*planey)
  local ex,ey=cell_center_world(exit_ix),cell_center_world(exit_iy)
  local rx=ex-player.x local ry=ey-player.y
  local tx=invdet*( diry*rx - dirx*ry)
  local ty=invdet*(-planey*rx + planex*ry)
  if ty>0 then
    local sx=flr((scr_w/2)*(1 + tx/ty))
    draw_sprite32_billboard(EXIT_SPR,ty,sx)
  end
end

function draw_enemies_billboards(dirx,diry,planex,planey)
  local invdet=1/(planex*diry - dirx*planey)
  local order={}
  for i=1,#enemies do
    local e=enemies[i]
    local rx=e.x-player.x local ry=e.y-player.y
    local tx=invdet*( diry*rx - dirx*ry)
    local ty=invdet*(-planey*rx + planex*ry)
    add(order,{i=i,ty=ty})
  end
  for a=1,#order do local bi=a for b=a+1,#order do if order[b].ty>order[bi].ty then bi=b end end if bi~=a then local t=order[a] order[a]=order[bi] order[bi]=t end end
  for k=1,#order do
    local e=enemies[order[k].i]
    local rx=e.x-player.x local ry=e.y-player.y
    local tx=invdet*( diry*rx - dirx*ry)
    local ty=invdet*(-planey*rx + planex*ry)
    if ty>0 then
      local sx=flr((scr_w/2)*(1 + tx/ty))
      draw_sprite32_billboard((e.t==0) and SPR_MELEE or SPR_RANGED, ty, sx)
    end
  end
end

function draw_projectiles(dirx,diry,planex,planey)
  local invdet=1/(planex*diry - dirx*planey)
  for pr in all(projectiles) do
    local rx=pr.x-player.x local ry=pr.y-player.y
    local tx=invdet*( diry*rx - dirx*ry)
    local ty=invdet*(-planey*rx + planex*ry)
    if ty>0 then
      local h=flr(scr_h/ty) local w=h\2
      local cx=flr((scr_w/2)*(1 + tx/ty))
      local x0=cx - w\2 local y0=half_h - h\2
      local xs=max(0,x0) local xe=min(127,x0+w-1)
      local col=(pr.from==1) and col_proj_pl or col_proj_en
      for x=xs,xe do if ty<zbuf[x] then line(x,y0, x, y0+h-1, col) end end
    end
  end
end

-- ===== HUD & screens =====
function draw_hud()
  local s="hp: " for i=1,3 do s..=(i<=player.hp and "\143" or "\151") end
  print(s, 2,2, player.hurt_cd>0 and col_hitfx or 7)
  print("FPS: "..stat(7), 80,100,7)
  print("CPU: "..stat(1), 80,108,7)
end

function draw_win() cls(0) print("\^wyou win!", 38,54,11) print("press ❎/🅾️ to play again", 18,70,6) end
function draw_dead() cls(0) print("\^woops!", 48,50,8) print("you died", 44,62,8) print("press ❎/🅾️ to retry", 22,78,6) end

function _draw()
  if game_state==1 then draw_win()
  elseif game_state==2 then draw_dead()
  else cls() draw_scene() draw_hud() end
end
