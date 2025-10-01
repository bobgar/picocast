-- pico-8 raycaster (32x32 textures, 50x50 rooms+corridors, robust collisions)
-- 0 = empty; >0 = wall (3 visual variants chosen at render)

scr_w,scr_h=128,128
half_h=64

-- constants
BIG=32767 -- pico-8 "infinity"

-- camera / movement
fov_deg=60
fov_turn=fov_deg/360
move_spd=0.135    -- fast
rot_spd=2.7/360   -- fast

-- textures: 32x32 (4x4 tiles)
tex_w,tex_h=32,32
num_textures=3

-- first texture top-left tile (tile coords). tile (1,0) == sprite #1
tex_start_tile_c=1
tex_start_tile_r=0

-- where to place the atlas in MAP (tile coords)
atlas_ay=0

-- colors
col_sky=12
col_floor=3

-- world size
W,H=50,50
lvlz=1
level={}

-- player + exit + state
p={x=3.5,y=3.5,a=0}
exit_ix,exit_iy=47,47
game_state=0           -- 0=play, 1=win

-- ===== utils =====
function irnd(a,b) return flr(rnd(b-a+1))+a end
function clamp(v,a,b) if v<a then return a elseif v>b then return b else return v end end
function sgn(x) if x<0 then return -1 elseif x>0 then return 1 else return 0 end end
function cell(x,y)
  if x<1 or y<1 or x>W or y>H then return 1 end
  return level[lvlz][y][x]
end
function setcell(x,y,v)
  if x>=1 and y>=1 and x<=W and y<=H then level[lvlz][y][x]=v end
end
function cell_center_world(i) return i-0.5 end

-- ===== build 32x32 -> MAP atlas (4x4 tiles), indices start at sprite #1 =====
function build_texture_atlas_in_map(n)
  local span=tex_w/8       -- 4 for 32x32
  local base_c=tex_start_tile_c
  local base_r=tex_start_tile_r
  for t=1,n do
    local src_c = base_c + (t-1)*span
    local src_r = base_r
    local ax=(t-1)*span
    local ay=atlas_ay
    for ty=0,span-1 do
      for tx=0,span-1 do
        local spr = (src_r+ty)*16 + (src_c+tx) -- sprite id (0..255)
        mset(ax+tx, ay+ty, spr)
      end
    end
  end
end

-- ===== corridor + rooms generation =====
function overlaps(rx,ry,rw,rh,rooms,pad)
  for r in all(rooms) do
    if not (rx+rw+pad < r.x-pad or r.x+r.w+pad < rx-pad
      or ry+rh+pad < r.y-pad or r.y+r.h+pad < ry-pad) then
      return true
    end
  end
  return false
end

function carve_rect(rx,ry,rw,rh)
  for y=ry,ry+rh do
    for x=rx,rx+rw do setcell(x,y,0) end
  end
end

function carve_corridor(ax,ay,bx,by)
  -- L-shaped corridor, order randomized
  if rnd() < 0.5 then
    local sx,ex= ax, bx if sx>ex then sx,ex=ex,sx end
    for x=sx,ex do setcell(x,ay,0) end
    local sy,ey= ay, by if sy>ey then sy,ey=ey,sy end
    for y=sy,ey do setcell(bx,y,0) end
  else
    local sy,ey= ay, by if sy>ey then sy,ey=ey,sy end
    for y=sy,ey do setcell(ax,y,0) end
    local sx,ex= ax, bx if sx>ex then sx,ex=ex,sx end
    for x=sx,ex do setcell(x,by,0) end
  end
end

function manhattan(x1,y1,x2,y2) return abs(x1-x2)+abs(y1-y2) end

function prim_connect(centers)
  local n=#centers
  if n<=1 then return end
  local in_tree={}
  local tree={1}
  in_tree[1]=true
  while #tree<n do
    local best_i,best_j,best_d=nil,nil,BIG
    for i in all(tree) do
      local a=centers[i]
      for j=1,n do
        if not in_tree[j] then
          local b=centers[j]
          local d=manhattan(a.x,a.y,b.x,b.y)
          if d<best_d then best_d=d best_i=i best_j=j end
        end
      end
    end
    local a,b=centers[best_i],centers[best_j]
    carve_corridor(a.x,a.y,b.x,b.y)
    add(tree,best_j) in_tree[best_j]=true
  end
end

function add_extra_links(centers,k)
  for i=1,k do
    local a=centers[irnd(1,#centers)]
    local b=centers[irnd(1,#centers)]
    if a~=b then carve_corridor(a.x,a.y,b.x,b.y) end
  end
end

function sprinkle_junctions(prob)
  for y=2,H-1 do
    for x=2,W-1 do
      if cell(x,y)==1 and rnd()<prob then
        local lr = (cell(x-1,y)==0 and cell(x+1,y)==0)
        local ud = (cell(x,y-1)==0 and cell(x,y+1)==0)
        if lr or ud then setcell(x,y,0) end
      end
    end
  end
end

function build_level()
  -- fill with walls
  local slice={}
  for y=1,H do
    local row={}
    for x=1,W do row[x]=1 end
    slice[y]=row
  end
  level={[lvlz]=slice}

  -- place rooms
  local rooms={}
  local centers={}
  local max_rooms=18
  local tries=0
  while #rooms<max_rooms and tries<500 do
    tries+=1
    local rw=irnd(6,12)
    local rh=irnd(6,12)
    local rx=irnd(3,W-2-rw)
    local ry=irnd(3,H-2-rh)
    if not overlaps(rx,ry,rw,rh,rooms,1) then
      carve_rect(rx,ry,rw,rh)
      local cx=rx+rw\2
      local cy=ry+rh\2
      add(rooms,{x=rx,y=ry,w=rw,h=rh,cx=cx,cy=cy})
      add(centers,{x=cx,y=cy})
    end
  end
  if #rooms==0 then
    carve_rect(10,10,30,30)
    add(centers,{x=25,y=25})
  end

  prim_connect(centers)
  add_extra_links(centers, #centers\3)
  sprinkle_junctions(0.06)

  -- start = room center closest to (3,3)
  local best_i=1 local best_d=BIG
  for i=1,#centers do
    local c=centers[i]
    local d=manhattan(c.x,c.y,3,3)
    if d<best_d then best_d=d best_i=i end
  end
  local s=centers[best_i]
  p.x=cell_center_world(s.x)
  p.y=cell_center_world(s.y)

  -- exit = farthest reachable empty cell from start
  choose_exit(s.x,s.y)
end

-- BFS for farthest reachable exit
function choose_exit(sx,sy)
  local dist={}
  for y=1,H do
    local row={}
    for x=1,W do row[x]=-1 end
    dist[y]=row
  end
  local q={{x=sx,y=sy}}
  local head=1
  dist[sy][sx]=0
  local fx,fy=sx,sy
  local best=0
  while head<=#q do
    local c=q[head] head+=1
    local d=dist[c.y][c.x]
    if d>best then best=d fx,fy=c.x,c.y end
    local dirs={{1,0},{-1,0},{0,1},{0,-1}}
    for di in all(dirs) do
      local nx,ny=c.x+di[1],c.y+di[2]
      if nx>=1 and ny>=1 and nx<=W and ny<=H and cell(nx,ny)==0 and dist[ny][nx]<0 then
        dist[ny][nx]=d+1
        add(q,{x=nx,y=ny})
      end
    end
  end
  exit_ix,exit_iy=fx,fy
end

-- ===== robust circle-vs-grid collision =====
local col_r=0.18 -- player radius (smaller = safer in tight corners)

-- push p out of any overlapping wall AABBs around it
function resolve_collisions()
  local px,py=p.x,p.y
  local ix,iy=flr(px)+1, flr(py)+1
  -- check neighborhood 3x3 around current cell
  for j=iy-1,iy+1 do
    for i=ix-1,ix+1 do
      if cell(i,j)>0 then
        -- wall AABB in world coords: [i-1,i] x [j-1,j]
        local minx,maxx=i-1,i
        local miny,maxy=j-1,j
        -- closest point on box to circle center
        local qx=mid(minx, px, maxx)
        local qy=mid(miny, py, maxy)
        local dx=px-qx
        local dy=py-qy
        local d2=dx*dx+dy*dy
        if d2 < col_r*col_r-0.0001 then
          if dx==0 and dy==0 then
            -- Center is inside box corner (rare): push along smallest overlap axis
            local left   = (px - minx)
            local right  = (maxx - px)
            local top    = (py - miny)
            local bottom = (maxy - py)
            local ox=min(left,right)
            local oy=min(top,bottom)
            if ox<oy then
              p.x += (left<right) and (-(col_r-left)) or (col_r-right)
            else
              p.y += (top<bottom) and (-(col_r-top)) or (col_r-bottom)
            end
          else
            local d=sqrt(d2)
            local push=(col_r - d)/max(0.0001,d)
            p.x += dx*push
            p.y += dy*push
          end
          -- update for next checks
          px,py=p.x,p.y
        end
      end
    end
  end
end

function unstick_if_inside()
  -- if center is inside a wall cell, pop to nearest open cell center
  local ix,iy=flr(p.x)+1, flr(p.y)+1
  if cell(ix,iy)==0 then return end
  for r=1,6 do
    for dy=-r,r do
      for dx=-r,r do
        local nx,ny=ix+dx,iy+dy
        if nx>=1 and ny>=1 and nx<=W and ny<=H and cell(nx,ny)==0 then
          p.x=cell_center_world(nx)
          p.y=cell_center_world(ny)
          return
        end
      end
    end
  end
end

function move_and_resolve(dx,dy)
  -- microsteps to avoid tunneling at higher speed
  local m=max(abs(dx),abs(dy))
  local steps=max(1,flr(m/0.05)+1)
  dx/=steps dy/=steps
  for i=1,steps do
    p.x+=dx
    p.y+=dy
    resolve_collisions()
  end
  unstick_if_inside()
end

-- ===== init & update =====
function _init()
    palt(0,false)
  -- srand(t()) -- uncomment for different layout per boot/session
  build_texture_atlas_in_map(num_textures)
  build_level()
end

function _update60()
  if game_state==1 then
    if btnp(4) or btnp(5) then build_level() game_state=0 end
    return
  end

  if btn(0) then p.a-=rot_spd end
  if btn(1) then p.a+=rot_spd end
  if p.a<0 then p.a+=1 end
  if p.a>=1 then p.a-=1 end

  local dirx,diry=cos(p.a),sin(p.a)
  local dx,dy=0,0
  if btn(2) then dx+=dirx*move_spd dy+=diry*move_spd end
  if btn(3) then dx-=dirx*move_spd dy-=diry*move_spd end
  if dx~=0 or dy~=0 then move_and_resolve(dx,dy) end

  -- win check
  local ex,ey=cell_center_world(exit_ix),cell_center_world(exit_iy)
  if (abs(p.x-ex)<0.4 and abs(p.y-ey)<0.4) then game_state=1 end
end

-- ===== renderer (Lode-style sampling; no near-wall stretch) =====
function draw_scene()
  local dirx,diry=cos(p.a),sin(p.a)
  local half_fov=fov_turn/2
  local t_half=sin(half_fov)/max(0.0001,cos(half_fov))
  local planex,planey=-diry*t_half, dirx*t_half

  rectfill(0,0,127,63,col_sky)
  rectfill(0,64,127,127,col_floor)

  local near_clip=0.02
  local span=tex_w/8 -- 4 for 32px

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

    local hit=0
    local side=0
    local guard=0
    while hit==0 and guard<512 do
      guard+=1
      if sdx<sdy then sdx+=ddx mapx+=stepx side=0 else sdy+=ddy mapy+=stepy side=1 end
      if cell(mapx+1,mapy+1)>0 then hit=1 end
    end

    local perp=(side==0) and (sdx-ddx) or (sdy-ddy)
    if perp<near_clip then perp=near_clip end

    if hit==1 then
      local line_h=flr(scr_h/perp)

      local draw_start=half_h - line_h\2
      local draw_end  =half_h + line_h\2
      draw_start=clamp(draw_start,0,scr_h-1)
      draw_end  =clamp(draw_end,0,scr_h-1)

      if draw_end>draw_start then
        -- exact grid intersection -> stable tex column
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
        if (side==0 and rdx>0) or (side==1 and rdy<0) then
          tex_x=tex_w-1-tex_x
        end

        -- vary textures along stretches
        local mxv=max(0,mapx)
        local myv=max(0,mapy)
        local tex_id = (side==0) and ((mxv%3)+1) or ((myv%3)+1)
        if tex_id>num_textures then tex_id=(tex_id-1)%num_textures+1 end

        -- Lode-style vertical sampling
        local step_px = tex_h/line_h
        local tex_pos = (draw_start - (half_h - line_h/2)) * step_px

        local u=(tex_id-1)*span + tex_x/8
        local v=atlas_ay + tex_pos/8
        local du=0
        local dv=step_px/8

        tline(x,draw_start, x,draw_end, u,v, du,dv)
      end
    end
  end
end

-- ===== HUD & win =====
function draw_hud()
  local ex,ey=cell_center_world(exit_ix),cell_center_world(exit_iy)
  local d = sqrt((p.x-ex)^2 + (p.y-ey)^2)
  d = flr(d*10)/10
  print("dist: "..d, 2,2,7)
  print("FPS: "..stat(7), 80,100,7)
  print("CPU: "..stat(1), 80,108,7)
end

function draw_win()
  cls(0)
  print("\^wyou win!", 38,54,11)
  print("press ❎/🅾️ to play again", 18,70,6)
end

function _draw()
  if game_state==1 then
    draw_win()
  else
    cls()
    draw_scene()
    draw_hud()
  end
end
