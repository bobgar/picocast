-- globals & constants
scr_w,scr_h=128,128
half_h=64
BIG=32767

-- fov/movement
fov_deg=60
fov_turn=fov_deg/360
move_spd=0.135
rot_spd=2.7/360

-- textures: 32x32 packed into map (4x4 sprite tiles)
tex_w,tex_h=32,32
span=tex_w/8

-- wall sources (sprite 0 is blank)
WALL_SRC0=1
WALL_SRC1=1+span
WALL_SRC2=1+span*2

-- doors (32x32), exit, key
DOOR_SRC=64
LOCKED_DOOR_SRC=76
EXIT_SPR=68
KEY_SPR=72

-- atlas
atlas_ay=0

-- door tile codes & timings
DOOR_CODE=9
LOCKED_DOOR_CODE=10
DOOR_OPEN_TIME=420
USE_RADIUS=1.0
DOOR_ANIM_FRAMES=30

-- palette
col_sky=12
col_floor=3
col_proj_en=8
col_proj_pl=10
col_hitfx=8

-- world, player, state
W,H=200,200
lvlz=1
level={}
rooms={}
centers={}
doors={}
door_map={}
player={x=3.5,y=3.5,a=0,hp=3,hurt_cd=0,fire_cd=0,has_key=false}
game_state=0
exit_ix,exit_iy=47,47
key_ent=nil

-- enemies & projectiles
enemies={}
projectiles={}
SPR_MELEE=128
SPR_RANGED=128+4

-- perf
plan_budget=1
