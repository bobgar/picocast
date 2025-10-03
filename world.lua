-- world.lua
-- map generation, doors, exit locking, key placement, spawning

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

function choose_exit(sx,sy)
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

-- exit room helpers
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

local function exit_room_entries(r)
  local entries={}
  for y=r.y-1,r.y+r.h+1 do
    for x=r.x-1,r.x+r.w+1 do
      if x>=2 and y>=2 and x<=W-1 and y<=H-1 then
        if cell(x,y)==0 then
          local u=cell(x,y-1)==0
          local d=cell(x,y+1)==0
          local l=cell(x-1,y)==0
          local r0=cell(x+1,y)==0
          if (u or d or l or r0) then
            if is_in_room(r,x+1,y) or is_in_room(r,x-1,y) or is_in_room(r,x,y+1) or is_in_room(r,x,y-1) then
              add(entries,{x=x,y=y})
            end
          end
        end
      end
    end
  end
  return entries
end

-- choose one entry as locked door, seal others
local function lock_exit_room_single_entry(startx,starty)
  local r=find_exit_room()
  if not r then return nil end
  local ents=exit_room_entries(r)
  if #ents==0 then return nil end

  local bi=1 local bd=-1
  for i=1,#ents do
    local e=ents[i]
    local d=abs(e.x-startx)+abs(e.y-starty)
    if d>bd then bd=d bi=i end
  end
  local gate=ents[bi]

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

  for i=1,#ents do
    if i~=bi then
      local e=ents[i]
      setcell(e.x,e.y,1)
      remove_door(e.x,e.y)
    end
  end

  return gate
end

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
  local min_start=8
  local min_gate=8
  for r in all(rooms) do
    for y=r.y+1,r.y+r.h-1 do
      for x=r.x+1,r.x+r.w-1 do
        if rs[y][x] and cell(x,y)==0 then
          local ds=abs(x-startx)+abs(y-starty)
          local dg=abs(x-gatex)+abs(y-gatey)
          if ds>=min_start and dg>=min_gate then
            local score=min(ds,dg) + (rnd()*0.25)
            if score>bestscore then bestscore=score best={x=x,y=y} end
          end
        end
      end
    end
  end
  if not best then
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
  for i=1,#centers do
    local c=centers[i] local d=manhattan(c.x,c.y,3,3)
    if d<bd then bd=d bi=i end
  end
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
