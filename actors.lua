-- projectiles, enemies, player fire (uses library.lua helpers)

function spawn_projectile(x,y,tx,ty,from)
 local dx,dy=tx-x,ty-y local l=max(0.0001,sqrt(dx*dx+dy*dy)) dx/=l dy/=l
 local sp=(from==1) and 0.22 or 0.12 local life=(from==1) and 60 or 90
 add(projectiles,{x=x,y=y,dx=dx,dy=dy,spd=sp,life=life,from=from})
end

local function move_projectile(pr)
 local tdx,tdy=pr.dx*pr.spd,pr.dy*pr.spd
 local steps=max(1,flr(max(abs(tdx),abs(tdy))/0.04)+1)
 local sdx,sdy=tdx/steps,tdy/steps
 for i=1,steps do pr.x+=sdx pr.y+=sdy if solid_at_world(pr.x,pr.y) then return true end end
end

function update_projectiles()
 for i=#projectiles,1,-1 do
  local pr=projectiles[i] local hit=move_projectile(pr) pr.life-=1
  if pr.life<=0 or hit then deli(projectiles,i)
  else
   if pr.from==0 then
    if dist(pr.x,pr.y,player.x,player.y)<0.4 then
     if player.hurt_cd<=0 then player.hp-=1 player.hurt_cd=30 if player.hp<=0 then game_state=2 end end
     deli(projectiles,i)
    end
   else
    for ei=#enemies,1,-1 do local e=enemies[ei]
     if dist(pr.x,pr.y,e.x,e.y) < e.r+0.25 then e.hp-=1 deli(projectiles,i) break end
    end
   end
  end
 end
end

function player_fire()
 if player.fire_cd>0 then return end
 local dx,dy=cos(player.a),sin(player.a)
 spawn_projectile(player.x+dx*0.25,player.y+dy*0.25, player.x+dx*1.25, player.y+dy*1.25, 1)
 player.fire_cd=10
end

local function enemy_try_open_doors(e)
 local best_i=-1 local best_d=30000
 for i=1,#doors do local d=doors[i]
  if not d.removed and (not d.locked) and (not d.open or d.anim<1) then
   local wx,wy=cell_center_world(d.x),cell_center_world(d.y)
   local dd=dist(wx,wy,e.x,e.y)
   if dd<1.0 and dd<best_d then best_d=dd best_i=i end
  end
 end
 if best_i~=-1 then local d=doors[best_i] d.open=true d.timer=DOOR_OPEN_TIME end
end

local function follow_path(e)
 if not e.path or not e.path_i or e.path_i>#e.path then return false end
 local node=e.path[e.path_i] local nx,ny=node.x,node.y
 if cell(nx,ny)==DOOR_CODE then
  local wx,wy=cell_center_world(nx),cell_center_world(ny)
  if dist(wx,wy,e.x,e.y)<1.0 then open_door_tile(nx,ny,false) end
 end
 local wx,wy=cell_center_world(nx),cell_center_world(ny)
 local dx,dy=wx-e.x,wy-e.y local d=sqrt(dx*dx+dy*dy)
 if d<0.1 then e.path_i+=1 return follow_path(e) end
 if d>0 then e.x,e.y=move_circle(e.x,e.y,dx/d*e.spd,dy/d*e.spd,e.r) return true end
end

local function replan_to_player(e)
 if e.repath_cd>0 then e.repath_cd-=1 return end
 if not e.had_los and not e.path then return end
 if plan_budget<=0 then return end
 plan_budget-=1 e.repath_cd=18
 local sx,sy=flr(e.x)+1,flr(e.y)+1 local tx,ty=flr(player.x)+1,flr(player.y)+1
 local p=astar(sx,sy,tx,ty,100,6000) if p then e.path=p e.path_i=1 end
end

function update_enemies()
 for i=#enemies,1,-1 do
  local e=enemies[i]
  local dx,dy=player.x-e.x,player.y-e.y local d2=dx*dx+dy*dy local d=max(0.0001,sqrt(d2))
  local ux,uy=dx/d,dy/d

  if e.los_cd<=0 then
   e.los=(d2<196) and los_dda(e.x,e.y,player.x,player.y,256) or false
   if e.los then e.had_los=true end
   e.los_cd=e.los and 2 or 4
  else e.los_cd-=1 end

  enemy_try_open_doors(e)

  if e.t==0 then -- melee
   if e.los then e.path=nil e.x,e.y=move_circle(e.x,e.y,ux*e.spd,uy*e.spd,e.r)
   elseif e.had_los then if not follow_path(e) then replan_to_player(e) end end
   e.mcd=max(0,(e.mcd or 0)-1)
   if d<0.75 and e.mcd<=0 then
    if player.hurt_cd<=0 then player.hp-=1 player.hurt_cd=30 if player.hp<=0 then game_state=2 end end
    e.mcd=14
   end
  else -- ranged
   if e.los then
    e.path=nil
    if d>6 then e.x,e.y=move_circle(e.x,e.y,ux*e.spd,uy*e.spd,e.r)
    elseif d<2 then e.x,e.y=move_circle(e.x,e.y,-ux*e.spd,-uy*e.spd,e.r) end
    e.cd=max(0,(e.cd or 0)-1)
    if d<12 and e.cd<=0 then spawn_projectile(e.x,e.y,player.x,player.y,0) e.cd=52 end
   elseif e.had_los then if not follow_path(e) then replan_to_player(e) end end
  end

  if e.hp<=0 then deli(enemies,i) end
 end
end
