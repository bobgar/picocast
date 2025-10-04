-- world.lua (10 buffered rooms, short halls, doors on hallway tiles only)

-- ===== helpers =====
local function rect_overlap(a,b,pad)
  pad=pad or 0
  return not (a.x+a.w+pad<=b.x or b.x+b.w+pad<=a.x or a.y+a.h+pad<=b.y or b.y+b.h+pad<=a.y)
end

local function center_xy(r) return r.x + r.w\2, r.y + r.h\2 end
local function manhattan(x1,y1,x2,y2) return abs(x1-x2)+abs(y1-y2) end

local function carve_room(r)
  for y=r.y, r.y+r.h-1 do
    for x=r.x, r.x+r.w-1 do setcell(x,y,0) end
  end
end

-- mask of room tiles
local function room_mask(rs)
  local m={} for y=1,H do m[y]={} end
  for r in all(rs) do
    for y=r.y, r.y+r.h-1 do
      for x=r.x, r.x+r.w-1 do m[y][x]=true end
    end
  end
  return m
end

-- dilate by 1 (8-neigh): ensure 1-tile wall buffer around rooms
local function dilate1(m)
  local dm={} for y=1,H do dm[y]={} end
  for y=1,H do
    for x=1,W do
      if m[y][x] then
        for dy=-1,1 do for dx=-1,1 do
          local nx,ny=x+dx,y+dy
          if nx>=1 and ny>=1 and nx<=W and ny<=H then dm[ny][nx]=true end
        end end
      end
    end
  end
  return dm
end

-- BFS on wall space; 'blocked' is bool grid
local function bfs_path(sx,sy,tx,ty,blocked)
  if sx==tx and sy==ty then return {{x=sx,y=sy}} end
  if blocked[sy] and blocked[sy][sx] then return end
  local q={{x=sx,y=sy}} local head=1
  local vis,px,py={}, {}, {}
  for y=1,H do vis[y]={} px[y]={} py[y]={} end
  vis[sy][sx]=true
  while head<=#q do
    local c=q[head] head+=1
    for d in all(DIRS) do
      local nx,ny=c.x+d[1], c.y+d[2]
      if nx>=1 and ny>=1 and nx<=W and ny<=H and not vis[ny][nx] then
        if not (blocked[ny] and blocked[ny][nx]) then
          vis[ny][nx]=true px[ny][nx]=c.x py[ny][nx]=c.y
          if nx==tx and ny==ty then
            local path={{x=tx,y=ty}} local cx,cy=tx,ty local guard=0
            while not (cx==sx and cy==sy) do
              guard+=1 if guard>30000 then return nil end
              if not (px[cy] and py[cy] and px[cy][cx] and py[cy][cx]) then return nil end
              local nx2,ny2=px[cy][cx],py[cy][cx]
              add(path,{x=nx2,y=ny2},1) cx,cy=nx2,ny2
            end
            return path
          end
          add(q,{x=nx,y=ny})
        end
      end
    end
  end
end

-- pick a doorway on the room boundary and the immediate outside cell (no mask checks here)
local function pick_port(room, toward_x, toward_y)
  local rx,ry,rw,rh=room.x,room.y,room.w,room.h
  local cx,cy=center_xy(room)
  local sx=(toward_x<cx) and -1 or 1
  local sy=(toward_y<cy) and -1 or 1
  local sides=(rnd()<0.5) and {{sx,0},{0,sy},{-sx,0},{0,-sy}} or {{0,sy},{sx,0},{0,-sy},{-sx,0}}
  for si=1,4 do
    local dx,dy=sides[si][1],sides[si][2]
    if dx~=0 then
      local x=(dx<0) and rx or (rx+rw-1)
      for _=1,32 do
        local y=irnd(ry,ry+rh-1)
        local outx,outy=x+dx,y
        if outx>=1 and outx<=W and outy>=1 and outy<=H then
          return {x=x,y=y},{x=outx,y=outy}
        end
      end
    else
      local y=(dy<0) and ry or (ry+rh-1)
      for _=1,32 do
        local x=irnd(rx,rx+rw-1)
        local outx,outy=x,y+dy
        if outx>=1 and outx<=W and outy>=1 and outy<=H then
          return {x=x,y=y},{x=outx,y=outy}
        end
      end
    end
  end
end

local function carve_corridor_path(path)
  for i=1,#path do setcell(path[i].x,path[i].y,0) end
end

local function place_door(ix,iy,locked)
  local code=locked and LOCKED_DOOR_CODE or DOOR_CODE
  setcell(ix,iy,code)
  local d={x=ix,y=iy,open=false,anim=0,timer=0,locked=locked,removed=false}
  add(doors,d) door_map[dkey(ix,iy)]=#doors
end

-- ===== rooms (10) with a 1-tile buffer between them =====
local function make_rooms(n)
  rooms={}
  local minw,maxw=5,12
  local minh,maxh=5,12
  local tries=0
  while #rooms<n and tries<3000 do
    tries+=1
    local w=irnd(minw,maxw) local h=irnd(minh,maxh)
    local rx=irnd(3, W-w-2) local ry=irnd(3, H-h-2)
    local cand={x=rx,y=ry,w=w,h=h,kind='normal',id=#rooms+1}
    local ok=true
    -- pad=2 -> guarantees at least one wall tile between rectangles
    for r in all(rooms) do if rect_overlap(cand,r,2) then ok=false break end end
    if ok then add(rooms,cand) end
  end
  return #rooms==n
end

-- nearest-neighbor tree on centers
local function build_tree()
  local n=#rooms
  local inT={} inT[1]=true
  local edges={}
  while #edges<n-1 do
    local bi,bj,bd=nil,nil,30000
    for i=1,n do if inT[i] then
      local ax,ay=center_xy(rooms[i])
      for j=1,n do if not inT[j] then
        local bx,by=center_xy(rooms[j])
        local d=manhattan(ax,ay,bx,by)
        if d<bd then bd=d bi=i bj=j end
      end end
    end end
    if not bi then break end
    add(edges,{a=bi,b=bj,w=bd}) inT[bj]=true
  end
  return edges
end

local function build_adj(edges)
  local adj={} for i=1,#rooms do adj[i]={} end
  for e in all(edges) do add(adj[e.a],{to=e.b,w=e.w}) add(adj[e.b],{to=e.a,w=e.w}) end
  return adj
end

local function dijkstra(adj,src)
  local dist,prev={},{}
  for i=1,#rooms do dist[i]=30000 prev[i]=0 end
  dist[src]=0 local used={}
  for _=1,#rooms do
    local u=-1 local best=30001
    for i=1,#rooms do if not used[i] and dist[i]<best then best=dist[i] u=i end end
    if u==-1 then break end
    used[u]=true
    for e in all(adj[u]) do
      local v=e.to local nd=dist[u]+e.w
      if nd<dist[v] then dist[v]=nd prev[v]=u end
    end
  end
  return dist,prev
end

local function path_rooms(prev,a,b)
  local p={} local cur=b
  while cur~=0 and cur~=a do add(p,cur,1) cur=prev[cur] end
  if cur==a then add(p,a,1) return p end
end

-- carve MST corridors with 1-tile room buffer; put doors on first corridor tiles
local function connect_corridors(tree, exit_idx)
  -- carve rooms now
  for r in all(rooms) do carve_room(r) end
  local rmask=room_mask(rooms)
  local dmask=dilate1(rmask)

  for e in all(tree) do
    local ra,rb=rooms[e.a],rooms[e.b]
    local acx,acy=center_xy(ra) local bcx,bcy=center_xy(rb)

    -- ports (inside + immediate outside)
    local a_in,a_out=pick_port(ra,bcx,bcy)
    local b_in,b_out=pick_port(rb,acx,acy)
    if not a_in or not b_in then return false end

    -- open doorway floor inside rooms
    setcell(a_in.x,a_in.y,0) setcell(b_in.x,b_in.y,0)

    -- blocked = dilated rooms (1-tile buffer), but open holes at the two out cells
    local blocked={} for y=1,H do blocked[y]={} for x=1,W do blocked[y][x]=dmask[y][x] or false end end
    blocked[a_out.y][a_out.x]=false blocked[b_out.y][b_out.x]=false

    local path=bfs_path(a_out.x,a_out.y,b_out.x,b_out.y,blocked)
    if not path or #path<2 then
      -- undo doorway if failed (keep walls intact)
      setcell(a_in.x,a_in.y,1) setcell(b_in.x,b_in.y,1)
      return false
    end

    -- carve corridor path
    carve_corridor_path(path)

    -- door tiles = the first corridor tile at each end (guaranteed outside room)
    local a_door=path[1] local b_door=path[#path]
    -- robustness: if somehow inside a room, nudge one step inward
    if rmask[a_door.y] and rmask[a_door.y][a_door.x] and #path>=2 then a_door=path[2] end
    if rmask[b_door.y] and rmask[b_door.y][b_door.x] and #path>=2 then b_door=path[#path-1] end

    local lock_a=(e.a==exit_idx) local lock_b=(e.b==exit_idx)
    place_door(a_door.x,a_door.y, lock_a)
    place_door(b_door.x,b_door.y, lock_b)
  end
  return true
end

-- ===== public API =====
function random_pos_in_room(r)
  for _=1,60 do
    local x=irnd(r.x, r.x+r.w-1) local y=irnd(r.y, r.y+r.h-1)
    if cell(x,y)==0 then return cell_center_world(x), cell_center_world(y) end
  end
end

function spawn_enemies()
  enemies={}
  local cands={}
  for i=1,#rooms do if rooms[i].kind=='normal' then add(cands,i) end end
  for i=1,6 do
    if #cands==0 then break end
    local rid=cands[irnd(1,#cands)] local rx,ry=random_pos_in_room(rooms[rid])
    if rx and dist(rx,ry,player.x,player.y)>8 then
      add(enemies,{x=rx,y=ry,t=0,spd=0.045,r=0.18,cd=0,hp=3,los=false,los_cd=0,had_los=false,repath_cd=0,mcd=0})
    end
  end
  for i=1,4 do
    if #cands==0 then break end
    local rid=cands[irnd(1,#cands)] local rx,ry=random_pos_in_room(rooms[rid])
    if rx and dist(rx,ry,player.x,player.y)>10 then
      add(enemies,{x=rx,y=ry,t=1,spd=0.038,r=0.18,cd=0,hp=2,los=false,los_cd=0,had_los=false,repath_cd=0,mcd=0})
    end
  end
end

-- one build attempt
local function attempt_build()
  -- smaller map to validate structure easily
  W=80 H=80
  local slice={} for y=1,H do local row={} for x=1,W do row[x]=1 end slice[y]=row end
  level={[lvlz]=slice} doors={} door_map={} rooms={} centers={} key_ent=nil

  -- 1) rooms (10) with 1-tile spacing
  if not make_rooms(10) then return false end

  -- 2) nearest-neighbor MST
  local tree=build_tree() if #tree<#rooms-1 then return false end
  local adj=build_adj(tree)

  -- 3) start random; exit = farthest via tree
  local start_idx=irnd(1,#rooms)
  local dist_s,prev_s=dijkstra(adj,start_idx)
  local exit_idx=1 local best=-1
  for i=1,#rooms do if dist_s[i]>best then best=dist_s[i] exit_idx=i end end

  -- 4) key room maximizes min(dist to start, dist to exit)
  local dist_e,_=dijkstra(adj,exit_idx)
  local key_idx=1 local bestmin=-1
  for i=1,#rooms do
    if i~=start_idx and i~=exit_idx then
      local m=min(dist_s[i], dist_e[i]) if m>bestmin then bestmin=m key_idx=i end
    end
  end

  -- 5) prune exit to single entrance: keep only edge on path exit->start
  local path_to_start=path_rooms(prev_s,start_idx,exit_idx) or {exit_idx,start_idx}
  local keep_neighbor=path_to_start[2] or start_idx
  local pruned={}
  for e in all(tree) do
    if (e.a==exit_idx and e.b~=keep_neighbor) or (e.b==exit_idx and e.a~=keep_neighbor) then
    else add(pruned,e) end
  end
  tree=pruned

  -- 6) tag kinds & centers
  for i=1,#rooms do rooms[i].kind='normal' end
  rooms[start_idx].kind='start' rooms[exit_idx].kind='exit' rooms[key_idx].kind='key'
  centers={} for i=1,#rooms do local cx,cy=center_xy(rooms[i]) add(centers,{x=cx,y=cy}) end

  -- 7) carve corridors with buffer; lock exit-side door
  if not connect_corridors(tree, exit_idx) then return false end

  -- 8) place player/exit/key
  local sr,er,kr=rooms[start_idx],rooms[exit_idx],rooms[key_idx]
  player.x,player.y=cell_center_world(sr.x+sr.w\2), cell_center_world(sr.y+sr.h\2)
  player.hp=3 player.hurt_cd=0 player.fire_cd=0 player.has_key=false
  local ecx,ecy=center_xy(er) exit_ix,exit_iy=ecx,ecy
  key_ent={x=cell_center_world(kr.x+kr.w\2), y=cell_center_world(kr.y+kr.h\2), got=false}

  spawn_enemies() projectiles={}
  return true
end

function build_level()
  for _=1,24 do if attempt_build() then return end end
  -- fallback
  W=64 H=64
  local slice={} for y=1,H do local row={} for x=1,W do row[x]=1 end slice[y]=row end
  level={[lvlz]=slice} doors={} door_map={}
  rooms={{x=8,y=8,w=W-16,h=H-16,kind='start',id=1}}
  carve_room(rooms[1])
  player.x,player.y=cell_center_world(rooms[1].x+rooms[1].w\2), cell_center_world(rooms[1].y+rooms[1].h\2)
  exit_ix,exit_iy=rooms[1].x+rooms[1].w-3,rooms[1].y+rooms[1].h-3
  key_ent=nil enemies={} projectiles={}
end
