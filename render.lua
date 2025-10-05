-- raycaster + door overlay + billboards (lean)

zb={} local dov={}

function draw_scene()
 local dx,dy=cos(player.a),sin(player.a)
 local hf=fov_turn/2
 local t=sin(hf)/max(0.0001,cos(hf))
 local px,py=-dy*t,dx*t

 rectfill(0,0,127,63,col_sky) rectfill(0,64,127,127,col_floor)
 dov={} local nc=0.03

 for x=0,scr_w-1 do
  local cx=x/(scr_w-1)*2-1
  local rdx,rdy=dx+px*cx, dy+py*cx
  local mx,my=flr(player.x),flr(player.y)
  local ddx=(rdx==0) and BIG or abs(1/rdx)
  local ddy=(rdy==0) and BIG or abs(1/rdy)
  local sx,sy,sdx,sdy
  if rdx<0 then sx=-1 sdx=(player.x-mx)*ddx else sx=1 sdx=(mx+1-player.x)*ddx end
  if rdy<0 then sy=-1 sdy=(player.y-my)*ddy else sy=1 sdy=(my+1-player.y)*ddy end

  local hv,sd=0,0 local g=0
  while hv==0 and g<256 do
   g+=1
   if sdx<sdy then sdx+=ddx mx+=sx sd=0 else sdy+=ddy my+=sy sd=1 end
   local ix,iy=mx+1,my+1 local c=cell(ix,iy)
   if c==DOOR_CODE or c==LOCKED_DOOR_CODE then
    local d=door_at(ix,iy)
    if d then
     local p=(sd==0) and (sdx-ddx) or (sdy-ddy) if p<nc then p=nc end
     local tx=tex_x32(sd,mx,my,sx,sy,rdx,rdy,player.x,player.y)
     if d.anim>0 then
      local ub=((c==LOCKED_DOOR_CODE) and 4 or 3)*span
      local ov=dov[x]
      if (not ov) or (p<ov.p) then dov[x]={p=p,tx=tx,a=d.anim,u=ub} end
     else hv=c end
    end
   elseif c>0 then hv=c end
  end

  local p=(sd==0) and (sdx-ddx) or (sdy-ddy) if p<nc then p=nc end
  zb[x]=p
  if hv>0 then
   local lh=flr(scr_h/p)
   local ys=clamp(half_h-lh\2,0,scr_h-1)
   local ye=clamp(ys+lh-1,0,scr_h-1)
   local tx=tex_x32(sd,mx,my,sx,sy,rdx,rdy,player.x,player.y)
   local base=((sd==0) and max(0,mx) or max(0,my))%3
   local sel=(hv==DOOR_CODE) and 3 or ((hv==LOCKED_DOOR_CODE) and 4 or base)
   if ye>ys then
    local sp=tex_h/lh
    local tp=(ys-(half_h-lh/2))*sp
    local u=sel*span+tx/8 local v=atlas_ay+tp/8
    tline(x,ys,x,ye, u,v, 0, sp/8)
   end
  end
 end

 -- door overlay (split)
 for x=0,127 do
  local ov=dov[x]
  if ov then
   local lh=flr(scr_h/ov.p)
   local ys=clamp(half_h-lh\2,0,scr_h-1)
   local ye=clamp(ys+lh-1,0,scr_h-1)
   local m=tex_w/2 local gh=flr(ov.a*m)
   if abs(ov.tx-m)>=gh and ye>ys then
    local sp=tex_h/lh
    local tp=(ys-(half_h-lh/2))*sp
    local u=ov.u+ov.tx/8 local v=atlas_ay+tp/8
    if ov.p<zb[x] then zb[x]=ov.p end
    tline(x,ys,x,ye, u,v, 0, sp/8)
   end
  end
 end

 -- cheap projector
 local inv=1/(px*dy - dx*py)
 local function pr(wx,wy)
  local rx,ry=wx-player.x,wy-player.y
  local tx=inv*(dy*rx - dx*ry)
  local ty=inv*(-py*rx + px*ry)
  return tx,ty
 end

 -- key
 if key_ent and not key_ent.got then
  local tx,ty=pr(key_ent.x,key_ent.y)
  if ty>0 then
   local cx=flr((scr_w/2)*(1+tx/ty))
   if cx>=0 and cx<scr_w and ty<zb[cx] then
    draw_sprite32_billboard(KEY_SPR,ty,cx,0,0.10)
   end
  end
 end

 -- exit
 do
  local ex,ey=cell_center_world(exit_ix),cell_center_world(exit_iy)
  local tx,ty=pr(ex,ey)
  if ty>0 then
   local cx=flr((scr_w/2)*(1+tx/ty))
   if cx>=0 and cx<scr_w and ty<zb[cx] then
    draw_sprite32_billboard(EXIT_SPR,ty,cx,0,0.15)
   end
  end
 end

 -- enemies (no sort; z-cull at center column)
 for e in all(enemies) do
  local tx,ty=pr(e.x,e.y)
  if ty>0 then
   local cx=flr((scr_w/2)*(1+tx/ty))
   if cx>=0 and cx<scr_w and ty<zb[cx] then
    draw_sprite32_billboard((e.t==0) and SPR_MELEE or SPR_RANGED, ty, cx,0,0)
   end
  end
 end

 -- projectiles (small circles; z-cull)
 for prj in all(projectiles) do
  local tx,ty=pr(prj.x,prj.y)
  if ty>0 then
   local h=flr(scr_h/ty) local cx=flr((scr_w/2)*(1+tx/ty)) local r=max(1,h\8)
   if cx>=0 and cx<scr_w and ty<zb[cx] then
    circfill(cx,half_h,r,(prj.from==1) and col_proj_pl or col_proj_en)
   end
  end
 end
end
