-- pico-8 raycaster
-- exit room has single locked entrance; key in reachable room away from start & gate
-- enemies open normal doors on path; melee cadence; A* budgeted per frame
-- no scientific notation; 16-bit-safe constants

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
span=tex_w/8

-- walls (sprite #0 blank; use >=1)
WALL_SRC0=1
WALL_SRC1=1+span
WALL_SRC2=1+span*2

-- doors (32x32)
DOOR_SRC=64
LOCKED_DOOR_SRC=76

-- exit + key (32x32)
EXIT_SPR=68
KEY_SPR=72

-- atlas row for tline sampling
atlas_ay=0

-- door constants
DOOR_CODE=9
LOCKED_DOOR_CODE=10
DOOR_OPEN_TIME=420
USE_RADIUS=1.0
DOOR_ANIM_FRAMES=30

-- colors
col_sky=12
col_floor=3
col_proj_en=8
col_proj_pl=10
col_hitfx=8

-- world & player
W,H=50,50
lvlz=1
level={}
rooms={}
centers={}
doors={}      -- {x,y,open,anim,timer,locked,removed}
door_map={}
player={x=3.5,y=3.5,a=0,hp=3,hurt_cd=0,fire_cd=0,has_key=false}
game_state=0  -- 0 play,1 win,2 dead
exit_ix,exit_iy=47,47
key_ent=nil   -- {x,y,got}

-- enemies, projectiles
enemies={}    -- {x,y,t,spd,r,cd,hp,los,los_cd,had_los,path,path_i,repath_cd,mcd}
projectiles={}-- {x,y,dx,dy,spd,life,from}

-- enemy sprites (32x32 blocks)
SPR_MELEE=128
SPR_RANGED=128+4

-- per-frame A* budget (perf)
plan_budget=1

-- ===== utils =====
function irnd(a,b) return flr(rnd(b-a+1))+a end
function clamp(v,a,b) if v<a then return a elseif v>b then return b end return v end
function cell(x,y) if x<1 or y<1 or x>W or y>H then return 1 end return level[lvlz][y][x] end
function setcell(x,y,v) if x>=1 and y>=1 and x<=W and y<=H then level[lvlz][y][x]=v end end
function cell_center_world(i) return i-0.5 end
function dist(a,b,c,d) return sqrt((a-c)^2+(b-d)^2) end
function dkey(x,y) return x..","..y end
function is_in_room(r,x,y) return x>=r.x and x<=r.x+r.w and y>=r.y and y<=r.y+r.h end

function door_at(ix,iy)
  local i=door_map[dkey(ix,iy)]
  if i then
    local d=doors[i]
    if d and not d.removed and d.x==ix and d.y==iy then return d end
  end
end

-- solid test (doors solid unless fully open)
function solid_cell(ix,iy)
  local t=cell(ix,iy)
  if t==DOOR_CODE or t==LOCKED_DOOR_CODE then
    local d=door_at(ix,iy)
    if d and d.anim>=1 then return false end
  end
  return t>0
end
function solid_at_world(x,y) return solid_cell(flr(x)+1, flr(y)+1) end

-- pathability
function passable_player_bfs(ix,iy)
  local t=cell(ix,iy)
  if t==0 then return true end
  if t==DOOR_CODE then
    local d=door_at(ix,iy) return d and d.anim>=1
  end
  return false
end
function passable_ai(ix,iy)
  local t=cell(ix,iy)
  if t==0 then return true end
  if t==DOOR_CODE then return true end
  return false
end

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

-- corridor orientation (strict straight)
local function corridor_ori(ix,iy)
  local u=cell(ix,iy-1)==0
  local d=cell(ix,iy+1)==0
  local l=cell(ix-1,iy)==0
  local r=cell(ix+1,iy)==0
  if u and d and (not l) and (not r) then return 'v' end
  if l and r and (not u) and (not d) then return 'h' end
  return nil
end

local function adj_non_corridor(ix,iy)
  local dirs={{1,0},{-1,0},{0,1},{0,-1}}
  for d in all(dirs) do
    local nx,ny=ix+d[1],iy+d[2]
    if cell(nx,ny)==0 and corridor_ori(nx,ny)==nil then return true end
  end
  return false
end

-- place one normal door per corridor run at the room end
function place_doors_at_room_exits()
  doors={} door_map={}
  local vis={}
  for y=1,H do local row={} for x=1,W do row[x]=false end vis[y]=row end
  for y=2,H-1 do
    for x=2,W-1 do
      if cell(x,y)==0 and not vis[y][x] then
        local ori=corridor_ori(x,y)
        if ori then
          local seg={}
          local bdx,bdy=(ori=='h') and -1 or 0, (ori=='v') and -1 or 0
          local cx,cy=x,y
          while cell(cx,cy)==0 and corridor_ori(cx,cy)==ori do cx-=bdx cy-=bdy end
          cx+=bdx cy+=bdy
          local fdx,fdy=(ori=='h') and 1 or 0, (ori=='v') and 1 or 0
          while cell(cx,cy)==0 and corridor_ori(cx,cy)==ori and not vis[cy][cx] do
            add(seg,{x=cx,y=cy})
            vis[cy][cx]=true
            cx+=fdx cy+=fdy
          end
          if #seg>0 then
            local s1=seg[1]
            local s2=seg[#seg]
            local end1_ok=adj_non_corridor(s1.x,s1.y)
            local end2_ok=adj_non_corridor(s2.x,s2.y)
            local pick=nil
            if end1_ok and not end2_ok then pick=s1
            elseif end2_ok and not end1_ok then pick=s2
            elseif end1_ok and end2_ok then pick=s1 end
            if pick and not (pick.x==exit_ix and pick.y==exit_iy) then
              setcell(pick.x,pick.y,DOOR_CODE)
              local d={x=pick.x,y=pick.y,open=false,anim=0,timer=0,locked=false,removed=false}
              add(doors,d)
              door_map[dkey(pick.x,pick.y)]=#doors
            end
          end
        else
          vis[y][x]=true
        end
      end
    end
  end
end

-- ===== enforce: exit room has a single locked entrance =====
local function find_exit_room()
  for r in all(rooms) do
    if is_in_room(r,exit_ix,exit_iy) then return r end
  end
end

local function remove_door(ix,iy)
  local k=dkey(ix,iy)
  local di=door_map[k]
  if di then
    doors[di].removed=true
    door_map[k]=nil
  end
end

-- gather corridor cells that touch exit room boundary
local function exit_room_entries(r)
  local entries={}
  for y=r.y-1,r.y+r.h+1 do
    for x=r.x-1,r.x+r.w+1 do
      if x>=2 and y>=2 and x<=W-1 and y<=H-1 then
        if cell(x,y)==0 and corridor_ori(x,y)~=nil then
          -- touching room interior?
          local touch=false
          if is_in_room(r,x+1,y) or is_in_room(r,x-1,y) or is_in_room(r,x,y+1) or is_in_room(r,x,y-1) then
            touch=true
          end
          if touch then add(entries,{x=x,y=y}) end
        end
      end
    end
  end
  return entries
end

-- choose entry far from start to be the locked gate; seal others
local function lock_exit_room_single_entry(startx,starty)
  local r=find_exit_room()
  if not r then return nil end
  local ents=exit_room_entries(r)
  if #ents==0 then return nil end

  -- pick farthest from start
  local bi=1 local bd=-1
  for i=1,#ents do
    local e=ents[i]
    local d=abs(e.x-startx)+abs(e.y-starty)
    if d>bd then bd=d bi=i end
  end
  local gate=ents[bi]

  -- convert/upgrade to locked door
  local k=dkey(gate.x,gate.y)
  local di=door_map[k]
  if di then
    doors[di].locked=true doors[di].removed=false
    setcell(gate.x,gate.y,LOCKED_DOOR_CODE)
  else
    setcell(gate.x,gate.y,LOCKED_DOOR_CODE)
    local d={x=gate.x,y=gate.y,open=false,anim=0,timer=0,locked=true,removed=false}
    add(doors,d) door_map[k]=#doors
  end

  -- seal other entries
  for i=1,#ents do
    if i~=bi then
      local e=ents[i]
      setcell(e.x,e.y,1)
      remove_door(e.x,e.y)
    end
  end

  return gate
end

-- ===== BFS reachability avoiding locked doors, then key placement =====
local function reachable_from(startx,starty)
  local rs={}
  for y=1,H do rs[y]={} end
  local q={{x=startx,y=starty}} local head=1
  rs[starty][startx]=true
  while head<=#q do
    local c=q[head] head+=1
    local dirs={{1,0},{-1,0},{0,1},{0,-1}}
    for d in all(dirs) do
      local nx,ny=c.x+d[1],c.y+d[2]
      if nx>=1 and ny>=1 and nx<=W and ny<=H and not rs[ny][nx] and passable_player_bfs(nx,ny) then
        rs[ny][nx]=true add(q,{x=nx,y=ny})
      end
    end
  end
  return rs
end

local function place_key_in_reachable_room(startx,starty,gatex,gatey)
  local rs=reachable_from(startx,starty)
  local best=nil local bestscore=-1
  local min_start=8  -- at least this far from start
  local min_gate=8   -- and from gate
  for r in all(rooms) do
    for y=r.y+1,r.y+r.h-1 do
      for x=r.x+1,r.x+r.w-1 do
        if rs[y][x] and cell(x,y)==0 then
          local ds=abs(x-startx)+abs(y-starty)
          local dg=abs(x-gatex)+abs(y-gatey)
          if ds>=min_start and dg>=min_gate then
            local score=min(ds,dg) + (rnd()*0.25) -- break ties
            if score>bestscore then bestscore=score best={x=x,y=y} end
          end
        end
      end
    end
  end
  if not best then
    -- fallback: farthest reachable floor from start
    for y=2,H-1 do
      for x=2,W-1 do
        if rs[y][x] and cell(x,y)==0 then
          local s=abs(x-startx)+abs(y-starty)
          if s>bestscore then bestscore=s best={x=x,y=y} end
        end
      end
    end
  end
  if best then
    key_ent={x=cell_center_world(best.x),y=cell_center_world(best.y),got=false}
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
      add(enemies,{x=rx,y=ry,t=0,spd=0.045,r=0.18,cd=0,hp=3,los=false,los_cd=0,had_los=false,repath_cd=0,mcd=0})
    end
  end
  for i=1,4 do
    local rid=irnd(1,#rooms)
    local rx,ry=random_pos_in_room(rooms[rid])
    if rx and dist(rx,ry,player.x,player.y)>10 then
      add(enemies,{x=rx,y=ry,t=1,spd=0.038,r=0.18,cd=0,hp=2,los=false,los_cd=0,had_los=false,repath_cd=0,mcd=0})
    end
  end
end

function build_level()
  -- walls everywhere
  local slice={}
  for y=1,H do local row={} for x=1,W do row[x]=1 end slice[y]=row end
  level={[lvlz]=slice}

  rooms={} centers={}
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
  player.hp=3 player.hurt_cd=0 player.fire_cd=0 player.has_key=false

  choose_exit(s.x,s.y)
  place_doors_at_room_exits()

  -- enforce single locked entrance to the exit room
  local gate=lock_exit_room_single_entry(s.x,s.y) or {x=exit_ix,y=exit_iy}
  -- place a key reachable without crossing the locked gate
  place_key_in_reachable_room(s.x,s.y,gate.x,gate.y)

  spawn_enemies()
  projectiles={}
end

-- ===== collisions =====
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

-- ===== A* (bounded, budgeted) =====
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

-- ===== DDA LOS =====
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

-- ===== doors =====
function door_slot(ix,iy)
  return (cell(ix,iy)==LOCKED_DOOR_CODE) and 4 or 3
end

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

-- ===== enemies =====
function player_fire()
  if player.fire_cd>0 then return end
  local dirx,diry=cos(player.a),sin(player.a)
  local sx=player.x+dirx*0.25 local sy=player.y+diry*0.25
  spawn_projectile(sx,sy, sx+dirx, sy+diry, 1)
  player.fire_cd=10
end

function enemy_try_open_doors(e)
  -- nearby normal doors
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
          if dist(pr.x,pr.y,e.x,e.y) < e.r+0.25 then e.hp-=1 deli(projectiles,i) break end
        end
      end
    end
  end
end

-- ===== init & main loop =====
function _init()
  -- srand(t()) -- enable for fresh layout each boot
  build_atlas_from_sources({WALL_SRC0,WALL_SRC1,WALL_SRC2,DOOR_SRC,LOCKED_DOOR_SRC})
  build_level()
end

function _update60()
  plan_budget=1 -- at most one A* per frame
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

  if btnp(5) then player_fire() end
  if btnp(4) then try_open_door_at(player.x,player.y,true) end

  local dirx,diry=cos(player.a),sin(player.a)
  local dx,dy=0,0
  if btn(2) then dx+=dirx*move_spd dy+=diry*move_spd end
  if btn(3) then dx-=dirx*move_spd dy-=diry*move_spd end
  if dx~=0 or dy~=0 then player.x,player.y=move_circle(player.x,player.y,dx,dy,0.18) end

  -- key pickup
  if key_ent and not key_ent.got then
    if dist(player.x,player.y,key_ent.x,key_ent.y)<0.45 then key_ent.got=true player.has_key=true end
  end

  update_doors()
  update_enemies()
  update_projectiles()

  local ex,ey=cell_center_world(exit_ix),cell_center_world(exit_iy)
  if abs(player.x-ex)<0.4 and abs(player.y-ey)<0.4 then game_state=1 end
end

-- ===== renderer (unchanged except door overlay & billboards) =====
zbuf={}
local door_overlay={}

function draw_scene()
  local dirx,diry=cos(player.a),sin(player.a)
  local half_fov=fov_turn/2
  local t_half=sin(half_fov)/max(0.0001,cos(half_fov))
  local planex,planey=-diry*t_half, dirx*t_half

  rectfill(0,0,127,63,col_sky)
  rectfill(0,64,127,127,col_floor)

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
      if t==DOOR_CODE or t==LOCKED_DOOR_CODE then
        local d=door_at(ix,iy)
        if d then
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
          tex_x=mid(0,tex_x,tex_w-1)
          if d.anim>0 then
            local ubase=(t==LOCKED_DOOR_CODE) and 4 or 3
            local ov=door_overlay[x]
            if (not ov) or (perp<ov.perp) then
              door_overlay[x]={perp=perp, tex_x=tex_x, anim=d.anim, ubase=ubase*span}
            end
          else
            hit_val=t
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
      tex_x=mid(0,tex_x,tex_w-1)
      local tex_sel
      if hit_val==DOOR_CODE then tex_sel=3
      elseif hit_val==LOCKED_DOOR_CODE then tex_sel=4
      else
        local mxv=max(0,mapx) local myv=max(0,mapy)
        tex_sel = (side==0) and (mxv%3) or (myv%3)
      end
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

  -- overlay opening doors
  for x=0,127 do
    local ov=door_overlay[x]
    if ov then
      local perp=ov.perp
      local line_h=flr(scr_h/perp)
      local draw_start=half_h - line_h\2
      local draw_end=draw_start + line_h - 1
      local midx=tex_w/2
      local gap_half=flr(ov.anim*midx)
      if abs(ov.tex_x - midx) >= gap_half then
        local ys = clamp(draw_start,0,scr_h-1)
        local ye = clamp(draw_end,0,scr_h-1)
        if ye>ys then
          local step_px = tex_h/line_h
          local tex_pos = (ys - (half_h - line_h/2)) * step_px
          local u=ov.ubase + ov.tex_x/8
          local v=atlas_ay + tex_pos/8
          if perp<zbuf[x] then zbuf[x]=perp end
          tline(x,ys, x,ye, u,v, 0, step_px/8)
        end
      end
    end
  end

  draw_key_billboard(dirx,diry,planex,planey)
  draw_exit_billboard(dirx,diry,planex,planey)
  draw_enemies_billboards(dirx,diry,planex,planey)
  draw_projectiles(dirx,diry,planex,planey)
end

-- ===== billboards =====
function draw_sprite32_billboard(tile_id, depth, screen_x, mode, y_off_frac)
  local s_tx=(tile_id%16)*8
  local s_ty=(tile_id\16)*8
  local src_x,src_y,src_w,src_h=s_tx,s_ty,32,32
  local h=flr(scr_h/depth) local w=h
  if w<1 or h<1 then return end
  local x0=flr(screen_x - w/2)
  local y0
  if mode==1 then y0 = 127 - h
  else
    local off=(y_off_frac or 0)*h
    y0 = flr(half_h - h/2 + off)
  end
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
    draw_sprite32_billboard(EXIT_SPR,ty,sx,0,0.15)
  end
end

function draw_key_billboard(dirx,diry,planex,planey)
  if not key_ent or key_ent.got then return end
  local invdet=1/(planex*diry - dirx*planey)
  local rx=key_ent.x-player.x local ry=key_ent.y-player.y
  local tx=invdet*( diry*rx - dirx*ry)
  local ty=invdet*(-planey*rx + planex*ry)
  if ty>0 then
    local sx=flr((scr_w/2)*(1 + tx/ty))
    draw_sprite32_billboard(KEY_SPR,ty,sx,0,0.10)
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
      draw_sprite32_billboard((e.t==0) and SPR_MELEE or SPR_RANGED, ty, sx, 0, 0)
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
      local h=flr(scr_h/ty)
      local cx=flr((scr_w/2)*(1 + tx/ty))
      local r=max(1,h\8)
      local cy=half_h
      if cx>=0 and cx<=127 and ty<zbuf[cx] then
        circfill(cx,cy,r,(pr.from==1) and col_proj_pl or col_proj_en)
      end
    end
  end
end

-- ===== HUD & screens =====
function draw_hud()
  local s="hp: " for i=1,3 do s..=(i<=player.hp and "\143" or "\151") end
  print(s, 2,2, player.hurt_cd>0 and col_hitfx or 7)
  if player.has_key then print("key", 2,10,11) end
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
