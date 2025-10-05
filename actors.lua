-- projectiles, enemies, player fire (token-lean)

function spawn_projectile(x,y,tx,ty,from)
 local dx,dy=tx-x,ty-y
 local il=1/max(0.0001,sqrt(dx*dx+dy*dy))
 add(projectiles,{
  x=x,y=y,dx=dx*il,dy=dy*il,
  spd=(from==1) and 0.22 or 0.12,
  life=(from==1) and 60 or 90,
  from=from
 })
end

local function move_projectile(p)
 local sx,sy=p.dx*p.spd/3,p.dy*p.spd/3
 for i=1,3 do
  p.x+=sx p.y+=sy
  if solid_at_world(p.x,p.y) then return true end
 end
end

function update_projectiles()
 for i=#projectiles,1,-1 do
  local p=projectiles[i]
  local hit=move_projectile(p) p.life-=1
  if p.life<=0 or hit then
    deli(projectiles,i)
  else
    if p.from==0 then
      local dx,dy=player.x-p.x,player.y-p.y
      if dx*dx+dy*dy<0.16 then
        if player.hurt_cd<=0 then player.hp-=1 player.hurt_cd=30 if player.hp<=0 then game_state=2 end end
        deli(projectiles,i)
      end
    else
      for ei=#enemies,1,-1 do
        local e=enemies[ei]
        local dx,dy=e.x-p.x,e.y-p.y
        local r=e.r+0.25
        if dx*dx+dy*dy<r*r then e.hp-=1 deli(projectiles,i) break end
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

local function follow_path(e)
 local p=e.path if not p or not e.path_i or e.path_i>#p then return false end
 local nd=p[e.path_i] local nx,ny=nd.x,nd.y
 if cell(nx,ny)==DOOR_CODE then
  local wx,wy=nx-0.5,ny-0.5
  local dx,dy=wx-e.x,wy-e.y
  if dx*dx+dy*dy<1 then open_door_tile(nx,ny,false) end
 end
 local wx,wy=nx-0.5,ny-0.5
 local dx,dy=wx-e.x,wy-e.y local d2=dx*dx+dy*dy
 if d2<0.01 then e.path_i+=1 return true end
 local il=1/max(0.0001,sqrt(d2))
 e.x,e.y=move_circle(e.x,e.y,dx*il*e.spd,dy*il*e.spd,e.r)
 return true
end

local function replan_to_player(e)
 if e.repath_cd>0 then e.repath_cd-=1 return end
 if not e.had_los and not e.path then return end
 if plan_budget<=0 then return end
 plan_budget-=1 e.repath_cd=18
 local sx,sy=flr(e.x)+1,flr(e.y)+1
 local tx,ty=flr(player.x)+1,flr(player.y)+1
 local p=astar(sx,sy,tx,ty,100,6000)
 if p then e.path=p e.path_i=1 end
end

function update_enemies()
 for i=#enemies,1,-1 do
  local e=enemies[i]
  local dx,dy=player.x-e.x,player.y-e.y
  local d2=dx*dx+dy*dy
  local il=1/max(0.0001,sqrt(d2))
  local ux,uy=dx*il,dy*il

  -- LOS throttle
  if e.los_cd<=0 then
    e.los=(d2<196) and los_dda(e.x,e.y,player.x,player.y,256) or false
    if e.los then e.had_los=true end
    e.los_cd=e.los and 2 or 4
  else e.los_cd-=1 end

  -- open near doors (3x3) to keep moving without scanning all doors
  local ix,iy=flr(e.x)+1,flr(e.y)+1
  for jy=iy-1,iy+1 do
    for ix2=ix-1,ix+1 do
      if cell(ix2,jy)==DOOR_CODE then
        local dx2,dy2=e.x-(ix2-0.5),e.y-(jy-0.5)
        if dx2*dx2+dy2*dy2<1 then open_door_tile(ix2,jy,false) end
      end
    end
  end

  if e.t==0 then -- melee
    if e.los then
      e.path=nil
      e.x,e.y=move_circle(e.x,e.y,ux*e.spd,uy*e.spd,e.r)
    elseif e.had_los then
      if not follow_path(e) then replan_to_player(e) end
    end
    e.mcd=max(0,(e.mcd or 0)-1)
    if d2<0.5625 and e.mcd<=0 then
      if player.hurt_cd<=0 then player.hp-=1 player.hurt_cd=30 if player.hp<=0 then game_state=2 end end
      e.mcd=14
    end
  else -- ranged
    if e.los then
      e.path=nil
      if d2>36 then
        e.x,e.y=move_circle(e.x,e.y,ux*e.spd,uy*e.spd,e.r)
      elseif d2<4 then
        e.x,e.y=move_circle(e.x,e.y,-ux*e.spd,-uy*e.spd,e.r)
      end
      e.cd=max(0,(e.cd or 0)-1)
      if d2<144 and e.cd<=0 then spawn_projectile(e.x,e.y,player.x,player.y,0) e.cd=52 end
    elseif e.had_los then
      if not follow_path(e) then replan_to_player(e) end
    end
  end

  if e.hp<=0 then deli(enemies,i) end
 end
end
