-- shared helpers (DRY + token-lean)

DIRS={{1,0},{-1,0},{0,1},{0,-1}}

-- math/tiles
function irnd(a,b) return flr(rnd(b-a+1))+a end
function clamp(v,a,b) return (v<a) and a or ((v>b) and b or v) end
function cell(x,y) if x<1 or y<1 or x>W or y>H then return 1 end return level[lvlz][y][x] end
function setcell(x,y,v) if x>=1 and y>=1 and x<=W and y<=H then level[lvlz][y][x]=v end end
function cell_center_world(i) return i-0.5 end
function dist(ax,ay,bx,by) return sqrt((ax-bx)^2+(ay-by)^2) end
function dkey(x,y) return x..","..y end
function is_in_room(r,x,y) return x>=r.x and x<=r.x+r.w and y>=r.y and y<=r.y+r.h end

-- doors & solids
function door_at(ix,iy)
 local i=door_map[dkey(ix,iy)]
 if i then local d=doors[i] if d and not d.removed and d.x==ix and d.y==iy then return d end end
end

function solid_cell(ix,iy)
 local t=cell(ix,iy)
 if t==DOOR_CODE or t==LOCKED_DOOR_CODE then local d=door_at(ix,iy) if d and d.anim>=1 then return false end end
 return t>0
end

function solid_at_world(x,y) return solid_cell(flr(x)+1,flr(y)+1) end

function passable_player_bfs(ix,iy)
 local t=cell(ix,iy)
 if t==0 then return true end
 if t==DOOR_CODE then local d=door_at(ix,iy) return d and d.anim>=1 end
 return false
end

function passable_ai(ix,iy)
 local t=cell(ix,iy)
 return (t==0) or (t==DOOR_CODE)
end

-- collisions (circle vs grid)
function resolve_circle(x,y,r)
 local px,py=x,y local r2=r*r
 local ix,iy=flr(px)+1, flr(py)+1
 for j=iy-1,iy+1 do
  for i=ix-1,ix+1 do
   if solid_cell(i,j) then
    local minx,maxx=i-1,i local miny,maxy=j-1,j
    local qx, qy = mid(minx,px,maxx), mid(miny,py,maxy)
    local dx,dy=px-qx,py-qy local d2=dx*dx+dy*dy
    if d2<r2-0.0001 then
      if dx==0 and dy==0 then
        local l=px-minx local rgt=maxx-px local t=py-miny local b=maxy-py
        if min(l,rgt)<min(t,b) then px+=(l<rgt) and (-(r-l)) or (r-rgt) else py+=(t<b) and (-(r-t)) or (r-b) end
      else
        local d=sqrt(d2) local k=(r-d)/max(0.0001,d) px+=dx*k py+=dy*k
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
   for oy=-rr,rr do for ox=-rr,rr do
    local nx,ny=ix+ox,iy+oy
    if nx>=1 and ny>=1 and nx<=W and ny<=H and not solid_cell(nx,ny) then
     return cell_center_world(nx),cell_center_world(ny)
    end
   end end
  end
 end
 return x,y
end

-- LOS (doors block unless open)
function los_dda(ax,ay,bx,by,limit)
 local rdx,rdy=bx-ax,by-ay local len=max(0.0001,sqrt(rdx*rdx+rdy*rdy)) rdx/=len rdy/=len
 local mapx,mapy=flr(ax),flr(ay)
 local ddx=(rdx==0) and BIG or abs(1/rdx)
 local ddy=(rdy==0) and BIG or abs(1/rdy)
 local stepx,stepy,sdx,sdy
 if rdx<0 then stepx=-1 sdx=(ax-mapx)*ddx else stepx=1 sdx=(mapx+1-ax)*ddx end
 if rdy<0 then stepy=-1 sdy=(ay-mapy)*ddy else stepy=1 sdy=(mapy+1-ay)*ddy end
 local g=0 local lim=limit or 256
 while g<lim do
  g+=1
  if sdx<sdy then sdx+=ddx mapx+=stepx else sdy+=ddy mapy+=stepy end
  local ix,iy=mapx+1,mapy+1 local t=cell(ix,iy)
  if t>0 then
   if t==DOOR_CODE or t==LOCKED_DOOR_CODE then local d=door_at(ix,iy) if not(d and d.anim>=1) then return false end
   else return false end
  end
  if mapx==flr(bx) and mapy==flr(by) then return true end
 end
 return true
end

-- A* (bounded)  — kept compact; agents rely on it
function astar(sx,sy,tx,ty,rad,max_iter)
 rad=rad or 100 max_iter=max_iter or 6000
 local minx=max(1,min(sx,tx)-rad) local maxx=min(W,max(sx,tx)+rad)
 local miny=max(1,min(sy,ty)-rad) local maxy=min(H,max(sy,ty)+rad)
 local w,h=maxx-minx+1, maxy-miny+1
 local function inb(x,y) return x>=minx and y>=miny and x<=maxx and y<=maxy end
 if not inb(tx,ty) or not inb(sx,sy) then return end
 local function lx(x) return x-minx+1 end local function ly(y) return y-miny+1 end
 local g,px,py,closed,open={}, {}, {}, {}, {}
 for j=1,h do g[j]={} px[j]={} py[j]={} closed[j]={} for i=1,w do g[j][i]=30000 px[j][i]=0 py[j][i]=0 closed[j][i]=false end end
 local ox,oy=lx(sx),ly(sy) local txl,tyl=lx(tx),ly(ty) g[oy][ox]=0
 add(open,{x=ox,y=oy,g=0,f=abs(ox-txl)+abs(oy-tyl)}) local it=0
 while #open>0 and it<max_iter do
  it+=1
  local bi=1 for i=2,#open do if open[i].f<open[bi].f then bi=i end end
  local cur=open[bi] deli(open,bi)
  if not closed[cur.y][cur.x] then
   closed[cur.y][cur.x]=true
   if cur.x==txl and cur.y==tyl then
    local path={} local cx,cy=cur.x,cur.y
    while not(cx==ox and cy==oy) do
     add(path,{x=cx+minx-1,y=cy+miny-1},1)
     local nx=px[cy][cx] local ny=py[cy][cx] cx,cy=nx,ny
    end
    return path
   end
   for d in all(DIRS) do
    local nx,ny=cur.x+d[1],cur.y+d[2]
    if nx>=1 and ny>=1 and nx<=w and ny<=h and not closed[ny][nx] then
     local gx,gy=nx+minx-1,ny+miny-1
     if passable_ai(gx,gy) then
      local tg=cur.g+1
      if tg<g[ny][nx] then
       g[ny][nx]=tg px[ny][nx]=cur.x py[ny][nx]=cur.y
       add(open,{x=nx,y=ny,g=tg,f=tg+abs(nx-txl)+abs(ny-tyl)})
      end
     end
    end
   end
  end
 end
end

-- doors api
function open_door_tile(ix,iy,allow_locked)
 local d=door_at(ix,iy) if not d or d.removed then return false end
 if d.locked and not allow_locked then return false end
 d.open=true d.timer=DOOR_OPEN_TIME return true
end

function try_open_door_at(x,y,require_key)
 local bi=-1 local bd=30000
 for i=1,#doors do
  local d=doors[i]
  if not d.removed and (not d.open or d.anim<1) then
   local wx,wy=cell_center_world(d.x),cell_center_world(d.y)
   local dd=dist(wx,wy,x,y)
   if dd<USE_RADIUS and dd<bd then bd=dd bi=i end
  end
 end
 if bi==-1 then return false end
 local d=doors[bi]
 if d.locked and require_key and not player.has_key then return false end
 d.open=true d.timer=DOOR_OPEN_TIME return true
end

function update_doors()
 local step=1/DOOR_ANIM_FRAMES
 for d in all(doors) do
  if not d.removed then
   if d.open then
    if d.anim<1 then d.anim=min(1,d.anim+step) end
    if d.timer>0 then d.timer-=1 end
    if d.timer<=0 then
     local wx,wy=cell_center_world(d.x),cell_center_world(d.y)
     if dist(wx,wy,player.x,player.y)>0.45 then
      local ok=true for e in all(enemies) do if dist(wx,wy,e.x,e.y)<0.45 then ok=false break end end
      if ok then d.open=false end
     end
    end
   elseif d.anim>0 then d.anim=max(0,d.anim-step) end
  end
 end
end

-- atlas
function build_atlas_from_sources(srcs)
 for i=1,#srcs do
  local src=srcs[i] local sc,sr=src%16,src\16 local ax=(i-1)*span
  for ty=0,span-1 do for tx=0,span-1 do mset(ax+tx,atlas_ay+ty,(sr+ty)*16+(sc+tx)) end end
 end
end

-- ray helper: 32px texcoord for current hit
function tex_x32(side,mx,my,sx,sy,rdx,rdy,px,py)
 local wx
 if side==0 then
  local dx=(mx-px+(1-sx)/2)
  local hy=py + dx*(rdy/(rdx==0 and 0.0001 or rdx))
  wx=hy-flr(hy)
 else
  local dy=(my-py+(1-sy)/2)
  local hx=px + dy*(rdx/(rdy==0 and 0.0001 or rdy))
  wx=hx-flr(hx)
 end
 local tx=flr(wx*tex_w)
 if (side==0 and rdx>0) or (side==1 and rdy<0) then tx=tex_w-1-tx end
 return mid(0,tx,tex_w-1)
end

-- billboard (uses global zb from render.lua)
function draw_sprite32_billboard(tile_id,depth,cx,mode,yoff)
 local s_tx=(tile_id%16)*8 local s_ty=(tile_id\16)*8
 local h=flr(scr_h/depth) local w=h if w<1 or h<1 then return end
 local x0=flr(cx-w/2)
 local y0=(mode==1) and (127-h) or flr(half_h-h/2+(yoff or 0)*h)
 local x1=x0+w-1 if x1<0 or x0>127 then return end
 local xs=max(0,x0) local xe=min(127,x1)
 for x=xs,xe do
  if depth<zb[x] then
   local u=flr((x-x0)*32/w)
   sspr(s_tx+u,s_ty,1,32,x,y0,1,h)
  end
 end
end

function bprint(str,x,y,c,scale)
    _str_to_sprite_sheet(str)
   
    local w = #str*4
    local h = 5
    pal(7,c)
    palt(0,true)
   
    sspr(0,0,w,h,x,y,w*scale,h*scale)
    pal()
       
    _restore_sprites_from_usermem()
end


function _str_to_sprite_sheet(str)
    _copy_sprites_to_usermem()
   
    _black_out_sprite_row()
    set_sprite_target()
    print(str,0,0,7)
    set_screen_target()
end

function set_sprite_target()
    poke(0x5f55,0x00)
end

function set_screen_target()
    poke(0x5f55,0x60)
end

function _copy_sprites_to_usermem()
    memcpy(0x4300,0x0,0x0200)
end

function _black_out_sprite_row()
    memset(0x0,0,0x0200)
end

function _restore_sprites_from_usermem()
    memcpy(0x0,0x4300,0x0200)
end

-- local function convert(n)  
--     if n == 0 then 
--         return " " 
--     elseif n == DOOR_CODE then 
--         return "D" 
--     elseif n==LOCKED_DOOR_CODE then 
--         return "L" 
--     else 
--         return "X"  
--     end  
-- end

-- -- dumps the grid; if mark_player=true, marks the player's tile as "PP"
-- function debug_dump_level(mark_player)
--   printh("=== level dump ===")
--   local px,py = flr(player.x+0.5), flr(player.y+0.5)

--   for y=1,H do
--     local row=""
--     for x=1,W do
--       local cellstr
--       if mark_player and x==px and y==py then
--         cellstr="P"
--       elseif exit_ix == x and exit_iy == y then
--         cellstr="S"
--       elseif flr(key_ent.x) ==x and flr(key_ent.y) == y then
--         cellstr="K"
--       else
--         local v=cell(x,y)
--         cellstr=convert(v)
--       end
--       row..=cellstr
--     end
--     printh(row)
--   end    
-- end
