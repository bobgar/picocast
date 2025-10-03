-- config.lua
-- global constants, state, and small utilities

scr_w,scr_h=128,128
half_h=64
BIG=32767

-- camera / movement
fov_deg=60
fov_turn=fov_deg/360
move_spd=0.135
rot_spd=2.7/360

-- textures: 32x32 (4x4 sprite tiles)
tex_w,tex_h=32,32
span=tex_w/8 -- 4 across in map cells

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

-- door codes & params
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
door_map={}   -- "x,y"->index
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

-- solid tests (doors solid unless fully open)
function solid_cell(ix,iy)
  local t=cell(ix,iy)
  if t==DOOR_CODE or t==LOCKED_DOOR_CODE then
    local d=door_at(ix,iy)
    if d and d.anim>=1 then return false end
  end
  return t>0
end
function solid_at_world(x,y) return solid_cell(flr(x)+1, flr(y)+1) end

-- passability
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
