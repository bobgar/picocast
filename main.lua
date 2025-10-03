-- main.lua
-- lifecycle, input, HUD, win/lose screens

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

  if btnp(5) then player_fire() end         -- ❎ shoot
  if btnp(4) then try_open_door_at(player.x,player.y,true) end -- 🅾️ open (locked needs key)

  local dirx,diry=cos(player.a),sin(player.a)
  local dx,dy=0,0
  if btn(2) then dx+=dirx*move_spd dy+=diry*move_spd end
  if btn(3) then dx-=dirx*move_spd dy-=diry*move_spd end
  if dx~=0 or dy~=0 then player.x,player.y=move_circle(player.x,player.y,dx,dy,0.18) end

  -- key pickup
  if key_ent and not key_ent.got then
    if dist(player.x,player.y,key_ent.x,key_ent.y)<0.45 then
      key_ent.got=true player.has_key=true
    end
  end

  update_doors()
  update_enemies()
  update_projectiles()

  local ex,ey=cell_center_world(exit_ix),cell_center_world(exit_iy)
  if abs(player.x-ex)<0.4 and abs(player.y-ey)<0.4 then game_state=1 end
end

local function draw_hud()
  local s="hp: " for i=1,3 do s..=(i<=player.hp and "\143" or "\151") end
  print(s, 2,2, player.hurt_cd>0 and col_hitfx or 7)
  if player.has_key then print("key", 2,10,11) end
  print("FPS: "..stat(7), 80,100,7)
  print("CPU: "..stat(1), 80,108,7)
end

local function draw_win() cls(0) print("\^wyou win!", 38,54,11) print("press ❎/🅾️ to play again", 18,70,6) end
local function draw_dead() cls(0) print("\^woops!", 48,50,8) print("you died", 44,62,8) print("press ❎/🅾️ to retry", 22,78,6) end

function _draw()
  if game_state==1 then draw_win()
  elseif game_state==2 then draw_dead()
  else cls() draw_scene() draw_hud() end
end
