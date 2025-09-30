-- pico-8 raycaster (tline + map-backed textures)
-- lode-style dda; 60° fov

scr_w,scr_h=128,128
half_h=64
tex_w,tex_h=16,16        -- each texture = 2x2 map tiles
fov_deg=60
fov_turn=fov_deg/360     -- pico-8 trig uses "turns"
move_spd=0.045
rot_spd=0.9/360

-- -------- level: level[z][y][x] (using z=1) ----------
level={
 { -- z=1
  {1,1,1,1,1,1,1,1,1,1,1},
  {1,0,0,0,0,0,0,0,0,0,1},
  {1,0,2,0,0,0,0,0,3,0,1},
  {1,0,0,0,0,0,0,0,0,0,1},
  {1,0,0,0,2,0,3,0,0,0,1},
  {1,0,0,0,0,0,0,0,0,0,1},
  {1,0,3,0,0,0,0,0,2,0,1},
  {1,0,0,0,0,0,0,0,0,0,1},
  {1,0,0,0,0,0,0,0,0,0,1},
  {1,1,1,1,1,1,1,1,1,1,1}
 }
}
lvlz=1
lvly=#level[lvlz]
lvlx=#level[lvlz][1]

function cell(x,y)
 if x<1 or y<1 or x>lvlx or y>lvly then return 1 end
 return level[lvlz][y][x]
end

-- -------- player ----------
p={x=3.5,y=3.5,a=0}

-- -------- test textures on sprite sheet ----------
function make_test_textures()
 for v=0,15 do
  for u=0,15 do
   -- tex #1 at (0,0): checker
   local c=((u>>2)&1)~=((v>>2)&1) and 13 or 7
   sset(u,v,c)
   -- tex #2 at (16,0): vertical stripes
   sset(16+u,v,(u&1)==0 and 12 or 6)
   -- tex #3 at (32,0): horizontal stripes
   sset(32+u,v,(v&1)==0 and 11 or 3)
  end
 end
end

-- mirror 16x16 textures into the MAP as 2x2 tiles
-- so tline() can sample them in tile space (8px per tile)
-- place texture t (1-based) at map tiles (ax,ay)=( (t-1)*2 , 0 )
function build_texture_atlas_in_map(num_textures)
 for t=1,num_textures do
  local sx=(t-1)*16   -- sprite-sheet pixel x
  local sy=0
  -- sprite indices (8x8 each)
  local n0=(sy\8)*16 + (sx\8)     -- top-left
  local n1=n0+1                   -- top-right
  local n2=n0+16                  -- bottom-left
  local n3=n0+17                  -- bottom-right
  local ax=(t-1)*2  -- atlas tile x on MAP
  local ay=0
  mset(ax,  ay,  n0)
  mset(ax+1,ay,  n1)
  mset(ax,  ay+1,n2)
  mset(ax+1,ay+1,n3)
 end
end

function _init()
 --make_test_textures()
 build_texture_atlas_in_map(3)
end

-- -------- movement ----------
function try_move(nx,ny)
 local r=0.2
 if cell(flr(nx-r)+1,flr(p.y)+1)==0 and cell(flr(nx+r)+1,flr(p.y)+1)==0 then
  p.x=nx
 end
 if cell(flr(p.x)+1,flr(ny-r)+1)==0 and cell(flr(p.x)+1,flr(ny+r)+1)==0 then
  p.y=ny
 end
end

function _update60()
 if btn(0) then p.a-=rot_spd end
 if btn(1) then p.a+=rot_spd end
 if p.a<0 then p.a+=1 end
 if p.a>=1 then p.a-=1 end
 local dx,dy=cos(p.a),sin(p.a)
 local mx,my=0,0
 if btn(2) then mx+=dx*move_spd my+=dy*move_spd end
 if btn(3) then mx-=dx*move_spd my-=dy*move_spd end
 if mx~=0 or my~=0 then try_move(p.x+mx,p.y+my) end
end

-- -------- core draw ----------
function draw_scene()
 -- sky & floor
 rectfill(0,0,127,63,12)
 rectfill(0,64,127,127,3)

 for x=0,scr_w-1 do
  -- camera ray
  local cam=(x/(scr_w-1)-0.5)     -- -0.5..+0.5
  local ray_a=p.a+cam*fov_turn
  local rdx,rdy=cos(ray_a),sin(ray_a)

  -- ray starts in this cell
  local mapx,mapy=flr(p.x),flr(p.y)

  -- delta distance to next x/y grid lines
  local ddx = (rdx==0) and 32767 or abs(1/rdx)
  local ddy = (rdy==0) and 32767 or abs(1/rdy)

  -- initial side distances
  local stepx,stepy
  local sdx,sdy
  if rdx<0 then
   stepx=-1
   sdx=(p.x-mapx)*ddx
  else
   stepx=1
   sdx=(mapx+1-p.x)*ddx
  end
  if rdy<0 then
   stepy=-1
   sdy=(p.y-mapy)*ddy
  else
   stepy=1
   sdy=(mapy+1-p.y)*ddy
  end

  -- DDA (per Lode)
  local hit=0
  local side=0
  local guard=0
  while hit==0 and guard<256 do
   guard+=1
   if sdx<sdy then
    sdx+=ddx
    mapx+=stepx
    side=0
   else
    sdy+=ddy
    mapy+=stepy
    side=1
   end
   hit=cell(mapx+1,mapy+1)
  end

  -- perpendicular distance (lode: sideDist - deltaDist)
  local perp
  if side==0 then perp=sdx-ddx else perp=sdy-ddy end
  if perp<0.0001 then perp=0.0001 end

  -- projected wall slice
  local line_h=flr(scr_h/perp)
  local y0=half_h - line_h\2
  local y1=half_h + line_h\2
  if y0<0 then y0=0 end
  if y1>scr_h-1 then y1=scr_h-1 end

  -- where on the wall did we hit? (0..1)
  local wallx
  if side==0 then wallx=p.y + perp*rdy else wallx=p.x + perp*rdx end
  wallx = wallx - flr(wallx)
  local tex_x=flr(wallx*tex_w)
  -- fix mirroring so faces align
  if (side==0 and rdx>0) or (side==1 and rdy<0) then
   tex_x=tex_w-1-tex_x
  end

  -- texture atlas placement on MAP (tile coords)
  -- each 16x16 texture uses 2x2 tiles; packed horizontally
  local tindex=hit                -- 1..N
  local ax=(tindex-1)*2           -- atlas tile x
  local ay=0                      -- atlas tile y

  if y1>y0 then
   -- tline uses MAP TILE units (8 px per 1.0)
   local u = ax + tex_x/8         -- fixed column inside the 2x2 tile block
   local v = ay                   -- start at top of the block
   local du=0
   local dv=(tex_h)/(y1-y0)/8     -- advance down the texture (in tile units)
   tline(x,y0, x,y1, u,v, du,dv)
  else
   -- 1px fallback (sample middle of texture row from sheet)
   local sx=(tindex-1)*16 + tex_x
   local sy=8
   pset(x,y0,sget(sx,sy))
  end
 end
end

-- mini-map (just for sanity)
function draw_minimap()
 local s=6
 for my=1,lvly do
  for mx=1,lvlx do
   rectfill(1+(mx-1)*s,1+(my-1)*s, mx*s,my*s, cell(mx,my)>0 and 5 or 0)
  end
 end
 circfill(1+p.x*s,1+p.y*s,1,10)
 line(1+p.x*s,1+p.y*s, 1+(p.x+cos(p.a)*0.8)*s, 1+(p.y+sin(p.a)*0.8)*s, 10)
end

function _draw()
 cls()
 draw_scene()
 draw_minimap()
 print("fov=60",1,120,7)
end
