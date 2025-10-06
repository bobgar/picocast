-- lifecycle, input, HUD, screens

local function bigprint(s,x,y,c) for dy=0,1 do for dx=0,1 do print(s,x+dx,y+dy,c) end end end

function _init()
 -- srand(t()) -- enable for fresh layout each boot
 build_atlas_from_sources({WALL_SRC0,WALL_SRC1,WALL_SRC2,DOOR_SRC,LOCKED_DOOR_SRC})
 game_state=-1 -- start screen
end

function _update60()
 plan_budget=1
 if game_state~=0 then
  if btnp(4) or btnp(5) then build_level() game_state=0 end
  return
 end

 player.hurt_cd=max(0,player.hurt_cd-1)
 player.fire_cd=max(0,player.fire_cd-1)

 -- fire / use
if btnp(5) then player_fire() end
if btnp(4) then try_open_door_at(player.x,player.y,true) end

-- movement: hold 🅾️ to strafe with ←/→
local cx,cy=cos(player.a),sin(player.a)
local f=(btn(2) and 1 or 0)-(btn(3) and 1 or 0)                  -- forward/back
local s=btn(4) and ((btn(1) and 1 or 0)-(btn(0) and 1 or 0)) or 0 -- strafe when holding 🅾️
if not btn(4) then                                               -- otherwise turn
 if btn(0) then player.a-=rot_spd end
 if btn(1) then player.a+=rot_spd end
 player.a%=1
end
local dx=cx*move_spd*f + cy*move_spd*s
local dy=cy*move_spd*f - cx*move_spd*s
if dx~=0 or dy~=0 then player.x,player.y=move_circle(player.x,player.y,dx,dy,0.18) end

 -- key pickup
 if key_ent and not key_ent.got and dist(player.x,player.y,key_ent.x,key_ent.y)<0.45 then
  key_ent.got=true player.has_key=true
 end

 -- health pickup
for i=#healths,1,-1 do
 local h=healths[i]
 if dist(player.x,player.y,h.x,h.y)<0.45 then
  player.hp=player.hp+1  --todo for right now no max health check (IE "if player.hp<3 then ... end")
  deli(healths,i)
 end
end


 update_doors() update_enemies() update_projectiles()

 -- exit check
 local ex,ey=cell_center_world(exit_ix),cell_center_world(exit_iy)
 if abs(player.x-ex)<0.4 and abs(player.y-ey)<0.4 then game_state=1 end
end

function _draw()
 if game_state==-1 then
  cls(0)
  bprint("bobgar's", 19, 5, 10, 3)
  bprint("bobgar's", 20, 6, 12, 3)
  bprint("pico cast", 15, 27, 8, 3)
  bprint("pico cast", 16, 28, 9, 3)
  spr(128,14,56,4,4) spr(68,50,56,4,4) spr(132,86,56,4,4)
  print("press ❎/🅾️ to start",22,104,11)
 elseif game_state==1 then
  cls(0) print("\^wyou win!",38,54,11) print("press ❎/🅾️ to play again",18,70,6)
 elseif game_state==2 then
  cls(0) print("\^woops!",48,50,8) print("you died",44,62,8) print("press ❎/🅾️ to retry",22,78,6)
 else
  cls()
  draw_scene()
  -- hud
  --local s="hp:"..hearts[player.hp+1]
  for i=1,player.hp do spr(13, i*10,5) end
  --print(s,2,2, player.hurt_cd>0 and 8 or 7)
  if player.has_key then spr(14,10,15) end --print("key",2,10,11) end
  --print("fps:" .. stat(7) .. "\ncpu:" .. stat(1) ,80,100,7)
 end
end
