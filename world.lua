-- world.lua (fixed prune; short halls via aimed ports; token-lean)

-- pick a floor tile inside room (fallback: center)
local function rndpos(r)
 for _=1,32 do
  local x=irnd(r.x,r.x+r.w-1) local y=irnd(r.y,r.y+r.h-1)
  if cell(x,y)==0 then return cell_center_world(x),cell_center_world(y) end
 end
 local cx,cy=cxy(r) return cell_center_world(cx),cell_center_world(cy)
end

function spawn_enemies()
 enemies={}
 local pool={}
 for i=1,#rooms do if rooms[i].kind=='normal' then add(pool,i) end end
 -- melee
 for i=1,6 do
  if #pool==0 then break end
  local id=pool[irnd(1,#pool)] local x,y=rndpos(rooms[id])
  if x and dist(x,y,player.x,player.y)>8 then
   add(enemies,{x=x,y=y,t=0,spd=0.045,r=0.18,cd=0,hp=3,los=false,los_cd=0,had_los=false,repath_cd=0,mcd=0})
  end
 end
 -- ranged
 for i=1,4 do
  if #pool==0 then break end
  local id=pool[irnd(1,#pool)] local x,y=rndpos(rooms[id])
  if x and dist(x,y,player.x,player.y)>10 then
   add(enemies,{x=x,y=y,t=1,spd=0.038,r=0.18,cd=0,hp=2,los=false,los_cd=0,had_los=false,repath_cd=0,mcd=0})
  end
 end
end


local function ov(a,b,p) p=p or 0
 return not(a.x+a.w+p<=b.x or b.x+b.w+p<=a.x or a.y+a.h+p<=b.y or b.y+b.h+p<=a.y)
end

local function cxy(r) return r.x+r.w\2,r.y+r.h\2 end
local function carve(r)
 for y=r.y,r.y+r.h-1 do
  for x=r.x,r.x+r.w-1 do setcell(x,y,0) end
 end
end

-- grid BFS on "blocked" bool grid (wall space)
local function grid_bfs(sx,sy,tx,ty,blk)
 if sx==tx and sy==ty then return {{x=sx,y=sy}} end
 if blk[sy] and blk[sy][sx] then return end
 local q={{x=sx,y=sy}} local h=1
 local vis,px,py={}, {}, {}
 for y=1,H do vis[y]={} px[y]={} py[y]={} end
 vis[sy][sx]=true
 while h<=#q do
  local c=q[h] h+=1
  for d in all(DIRS) do
   local nx,ny=c.x+d[1],c.y+d[2]
   if nx>=1 and ny>=1 and nx<=W and ny<=H and not vis[ny][nx] then
    if not (blk[ny] and blk[ny][nx]) then
     vis[ny][nx]=true px[ny][nx]=c.x py[ny][nx]=c.y
     if nx==tx and ny==ty then
      local p={{x=tx,y=ty}} local cx,cy=tx,ty
      while not(cx==sx and cy==sy) do
       local px1,py1=px[cy][cx],py[cy][cx] if not px1 then return end
       add(p,{x=px1,y=py1},1) cx,cy=px1,py1
      end
      return p
     end
     add(q,{x=nx,y=ny})
    end
   end
  end
 end
end

-- aimed port: pick boundary tile toward target (short halls; no loops)
local function port(r,tx,ty)
 local rx,ry,rw,rh=r.x,r.y,r.w,r.h
 local cx,cy=cxy(r)
 if abs(tx-cx)>=abs(ty-cy) then
  local dx=(tx<cx) and -1 or 1
  local x=(dx<0) and rx or (rx+rw-1)
  local y=mid(ry, flr(ty), ry+rh-1)
  return {x=x,y=y},{x=x+dx,y=y}
 else
  local dy=(ty<cy) and -1 or 1
  local y=(dy<0) and ry or (ry+rh-1)
  local x=mid(rx, flr(tx), rx+rw-1)
  return {x=x,y=y},{x=x,y=y+dy}
 end
end

local function door(ix,iy,lock)
 setcell(ix,iy, lock and LOCKED_DOOR_CODE or DOOR_CODE)
 local d={x=ix,y=iy,open=false,anim=0,timer=0,locked=lock,removed=false}
 add(doors,d) door_map[ix..","..iy]=#doors
end

-- 10 rooms with 1-tile spacing
local function make_rooms(n)
 rooms={}
 local tries=0
 while #rooms<n and tries<3000 do
  tries+=1
  local w,h=irnd(5,12),irnd(5,12)
  local rx,ry=irnd(3,W-w-2),irnd(3,H-h-2)
  local r={x=rx,y=ry,w=w,h=h,kind='normal',id=#rooms+1}
  local ok=true for q in all(rooms) do if ov(r,q,2) then ok=false break end end
  if ok then add(rooms,r) end
 end
 return #rooms==n
end

-- nearest-neighbor tree (short halls)
local function build_tree()
 local n=#rooms local inT={} inT[1]=true local es={}
 while #es<n-1 do
  local ai,bj,bd=nil,nil,30000
  for i=1,n do if inT[i] then
   local ax,ay=cxy(rooms[i])
   for j=1,n do if not inT[j] then
    local bx,by=cxy(rooms[j]) local d=abs(ax-bx)+abs(ay-by)
    if d<bd then bd=d ai=i bj=j end
   end end
  end end
  if not ai then break end
  add(es,{a=ai,b=bj}) inT[bj]=true
 end
 return es
end

-- unweighted BFS over rooms (edge count)
local function graph_bfs(es,src)
 local n=#rooms local adj={} for i=1,n do adj[i]={} end
 for e in all(es) do add(adj[e.a],e.b) add(adj[e.b],e.a) end
 local dist,prev={},{} for i=1,n do dist[i]=30000 prev[i]=0 end
 dist[src]=0 local q={src} local h=1
 while h<=#q do
  local u=q[h] h+=1
  for v in all(adj[u]) do
   if dist[v]==30000 then dist[v]=dist[u]+1 prev[v]=u add(q,v) end
  end
 end
 return dist,prev
end

-- single edge corridor with 1-tile buffer; doors at ends; lock exit side
local function carve_edge(a,b,lock_side,blk,rm)
 local ra,rb=rooms[a],rooms[b]
 local ax,ay=cxy(ra) local bx,by=cxy(rb)
 local ain,aout=port(ra,bx,by) local bin,bout=port(rb,ax,ay)
 if not ain or not bin then return end
 setcell(ain.x,ain.y,0) setcell(bin.x,bin.y,0)
 -- punch two holes, path, then restore block state
 local ao=blk[aout.y][aout.x] local bo=blk[bout.y][bout.x]
 blk[aout.y][aout.x]=false blk[bout.y][bout.x]=false
 local p=grid_bfs(aout.x,aout.y,bout.x,bout.y,blk)
 blk[aout.y][aout.x]=ao blk[bout.y][bout.x]=bo
 if not p or #p<2 then setcell(ain.x,ain.y,1) setcell(bin.x,bin.y,1) return end
 for i=1,#p do setcell(p[i].x,p[i].y,0) end
 local ad=p[1] local bd=p[#p]
 if rm[ad.y] and rm[ad.y][ad.x] and #p>1 then ad=p[2] end
 if rm[bd.y] and rm[bd.y][bd.x] and #p>1 then bd=p[#p-1] end
 door(ad.x,ad.y, lock_side==a) door(bd.x,bd.y, lock_side==b)
 return true
end

-- one attempt
local function attempt()
 W=80 H=80
 local s={} for y=1,H do local r={} for x=1,W do r[x]=1 end s[y]=r end
 level={[lvlz]=s} doors={} door_map={} rooms={} centers={} key_ent=nil

 if not make_rooms(10) then return end
 local tree=build_tree() if #tree<#rooms-1 then return end

 -- start random; exit = farthest (edges)
 local si=irnd(1,#rooms)
 local ds,ps=graph_bfs(tree,si)
 local ei,bd=1,-1 for i=1,#rooms do if ds[i]>bd then bd=ds[i] ei=i end end
 -- key = argmax min(dist to start, dist to exit)
 local de,_=graph_bfs(tree,ei)
 local ki,md=1,-1
 for i=1,#rooms do if i~=si and i~=ei then local m=min(ds[i],de[i]) if m>md then md=m ki=i end end end
 -- prune exit to one edge (keep neighbor toward start)  **FIX**
 local path={} local cur=ei while cur~=0 and cur~=si do add(path,cur,1) cur=ps[cur] end if cur==si then add(path,si,1) end
 local keep=path[max(1,#path-1)] -- neighbor just before exit
 local pr={} for e in all(tree) do if not((e.a==ei and e.b~=keep) or (e.b==ei and e.a~=keep)) then add(pr,e) end end
 tree=pr

 -- carve rooms & build masks
 for r in all(rooms) do carve(r) end
 local rm={} for y=1,H do rm[y]={} end
 for r in all(rooms) do for y=r.y,r.y+r.h-1 do for x=r.x,r.x+r.w-1 do rm[y][x]=true end end end
 local blk={} for y=1,H do blk[y]={} end
 for y=1,H do for x=1,W do
  if rm[y][x] then for dy=-1,1 do for dx=-1,1 do
   local nx,ny=x+dx,y+dy if nx>=1 and ny>=1 and nx<=W and ny<=H then blk[ny][nx]=true end
  end end end
 end end

 -- connect edges (doors both ends; lock exit side)
 for e in all(tree) do if not carve_edge(e.a,e.b, ei, blk, rm) then return end end

 centers={} for i=1,#rooms do local x,y=cxy(rooms[i]) add(centers,{x=x,y=y}) end
 -- place player/exit/key
 local sr,er,kr=rooms[si],rooms[ei],rooms[ki]
 player.x,player.y=cell_center_world(sr.x+sr.w\2),cell_center_world(sr.y+sr.h\2)
 player.hp=3 player.hurt_cd=0 player.fire_cd=0 player.has_key=false
 local ex,ey=cxy(er) exit_ix,exit_iy=ex,ey
 key_ent={x=cell_center_world(kr.x+kr.w\2),y=cell_center_world(kr.y+kr.h\2),got=false}

 spawn_enemies() projectiles={}
 return true
end

function build_level()
 for _=1,20 do if attempt() then return end end
 -- fallback
 W=64 H=64
 local s={} for y=1,H do local r={} for x=1,W do r[x]=1 end s[y]=r end
 level={[lvlz]=s} doors={} door_map={}
 rooms={{x=8,y=8,w=W-16,h=H-16,kind='start',id=1}} carve(rooms[1])
 player.x,player.y=cell_center_world(rooms[1].x+rooms[1].w\2),cell_center_world(rooms[1].y+rooms[1].h\2)
 exit_ix,exit_iy=rooms[1].x+rooms[1].w-3,rooms[1].y+rooms[1].h-3
 key_ent=nil enemies={} projectiles={}
end
