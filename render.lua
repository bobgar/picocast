-- raycaster + door overlay + billboards (uses library helpers)

zbuf={} local door_overlay={}

local function door_slot(ix,iy) return (cell(ix,iy)==LOCKED_DOOR_CODE) and 4 or 3 end

function draw_scene()
 local dirx,diry=cos(player.a),sin(player.a)
 local half_fov=fov_turn/2 local t_half=sin(half_fov)/max(0.0001,cos(half_fov))
 local planex,planey=-diry*t_half,dirx*t_half

 rectfill(0,0,127,63,col_sky) rectfill(0,64,127,127,col_floor)
 for i=0,127 do door_overlay[i]=nil end
 local near_clip=0.03

 for x=0,scr_w-1 do
  local camx=x/(scr_w-1)*2-1
  local rdx,rdy=dirx+planex*camx, diry+planey*camx
  local mapx,mapy=flr(player.x),flr(player.y)
  local ddx=(rdx==0) and BIG or abs(1/rdx)
  local ddy=(rdy==0) and BIG or abs(1/rdy)
  local stepx,stepy,sdx,sdy
  if rdx<0 then stepx=-1 sdx=(player.x-mapx)*ddx else stepx=1 sdx=(mapx+1-player.x)*ddx end
  if rdy<0 then stepy=-1 sdy=(player.y-mapy)*ddy else stepy=1 sdy=(mapy+1-player.y)*ddy end

  local hit_val,side=0,0 local guard=0
  while hit_val==0 and guard<256 do
   guard+=1
   if sdx<sdy then sdx+=ddx mapx+=stepx side=0 else sdy+=ddy mapy+=stepy side=1 end
   local ix,iy=mapx+1,mapy+1 local t=cell(ix,iy)
   if t==DOOR_CODE or t==LOCKED_DOOR_CODE then
    local d=door_at(ix,iy)
    if d then
     local perp=(side==0) and (sdx-ddx) or (sdy-ddy) if perp<near_clip then perp=near_clip end
     local tx=tex_x32(side,mapx,mapy,stepx,stepy,rdx,rdy,player.x,player.y)
     if d.anim>0 then
      local ubase=door_slot(ix,iy)*span local ov=door_overlay[x]
      if (not ov) or (perp<ov.perp) then door_overlay[x]={perp=perp,tex_x=tx,anim=d.anim,ubase=ubase} end
     else hit_val=t end
    end
   elseif t>0 then hit_val=t end
  end

  local perp=(side==0) and (sdx-ddx) or (sdy-ddy) if perp<near_clip then perp=near_clip end
  zbuf[x]=perp
  if hit_val>0 then
   local line_h=flr(scr_h/perp)
   local ys=clamp(half_h-line_h\2,0,scr_h-1)
   local ye=clamp(ys+line_h-1,0,scr_h-1)
   local tx=tex_x32(side,mapx,mapy,stepx,stepy,rdx,rdy,player.x,player.y)
   local sel=(hit_val==DOOR_CODE) and 3 or ((hit_val==LOCKED_DOOR_CODE) and 4 or (((side==0) and max(0,mapx) or max(0,mapy))%3))
   if ye>ys then
    local step_px=tex_h/line_h local tex_pos=(ys-(half_h-line_h/2))*step_px
    local u=sel*span+tx/8 local v=atlas_ay+tex_pos/8
    tline(x,ys,x,ye, u,v, 0, step_px/8)
   end
  end
 end

 -- overlay opening doors (sliding split)
 for x=0,127 do
  local ov=door_overlay[x]
  if ov then
   local line_h=flr(scr_h/ov.perp)
   local ys=clamp(half_h-line_h\2,0,scr_h-1)
   local ye=clamp(ys+line_h-1,0,scr_h-1)
   local midx=tex_w/2 local gap_half=flr(ov.anim*midx)
   if abs(ov.tex_x-midx)>=gap_half and ye>ys then
    local step_px=tex_h/line_h local tex_pos=(ys-(half_h-line_h/2))*step_px
    local u=ov.ubase+ov.tex_x/8 local v=atlas_ay+tex_pos/8
    if ov.perp<zbuf[x] then zbuf[x]=ov.perp end
    tline(x,ys,x,ye, u,v, 0, step_px/8)
   end
  end
 end

 -- billboards
 local invdet=1/(planex*diry - dirx*planey)
 local function proj(wx,wy)
  local rx,ry=wx-player.x,wy-player.y
  local tx=invdet*(diry*rx - dirx*ry)
  local ty=invdet*(-planey*rx + planex*ry)
  return tx,ty
 end

 -- key
 if key_ent and not key_ent.got then
  local tx,ty=proj(key_ent.x,key_ent.y)
  if ty>0 then draw_sprite32_billboard(KEY_SPR,ty,flr((scr_w/2)*(1+tx/ty)),0,0.10) end
 end

 -- exit
 do
  local ex,ey=cell_center_world(exit_ix),cell_center_world(exit_iy)
  local tx,ty=proj(ex,ey)
  if ty>0 then draw_sprite32_billboard(EXIT_SPR,ty,flr((scr_w/2)*(1+tx/ty)),0,0.15) end
 end

 -- enemies (back-to-front by ty)
 local ord={}
 for i=1,#enemies do
  local e=enemies[i] local tx,ty=proj(e.x,e.y) add(ord,{i=i,tx=tx,ty=ty})
 end
 for a=1,#ord do local bi=a for b=a+1,#ord do if ord[b].ty>ord[bi].ty then bi=b end end if bi~=a then local t=ord[a] ord[a]=ord[bi] ord[bi]=t end end
 for k=1,#ord do local e=enemies[ord[k].i] local tx,ty=ord[k].tx,ord[k].ty
  if ty>0 then draw_sprite32_billboard((e.t==0) and SPR_MELEE or SPR_RANGED, ty, flr((scr_w/2)*(1+tx/ty)),0,0) end
 end

 -- projectiles (small circles)
 for pr in all(projectiles) do
  local tx,ty=proj(pr.x,pr.y)
  if ty>0 then
   local h=flr(scr_h/ty) local cx=flr((scr_w/2)*(1+tx/ty)) local r=max(1,h\8)
   if cx>=0 and cx<=127 and ty<zbuf[cx] then circfill(cx,half_h,r,(pr.from==1) and col_proj_pl or col_proj_en) end
  end
 end
end
