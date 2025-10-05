-- globals & constants
scr_w,scr_h=128,128
half_h=64
BIG=32767

-- fov/movement (60° => 1/6; 2.7/360 => 0.0075)
fov_turn=1/6
move_spd=0.135
rot_spd=0.0075

-- textures: 32x32 packed (4x4 sprites)
tex_w,tex_h=32,32
span=4

-- doors/exits/keys (sprites)
EXIT_SPR=68
KEY_SPR=72

-- atlas
atlas_ay=0

-- door tile codes & timings
DOOR_CODE=9
LOCKED_DOOR_CODE=10
DOOR_OPEN_TIME=420
USE_RADIUS=1
DOOR_ANIM_FRAMES=30

-- palette
col_ceil=5
col_floor=13
col_proj_en=8
col_proj_pl=10
-- col_hitfx removed (unused); add back if you use it

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
SPR_RANGED=132

-- perf
plan_budget=1

-- keep these so the atlas builds (render expects 0..2 = walls, 3 = door, 4 = locked)
WALL_SRC0=1
WALL_SRC1=1+span
WALL_SRC2=1+span*2
DOOR_SRC=64
LOCKED_DOOR_SRC=76

HEALTH_SPR=136
healths={}