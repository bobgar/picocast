-- render.lua
-- atlas build, raycaster, door overlay, billboards

-- build 32x32 atlas rows from sprite sources
function build_atlas_from_sources(sources)
  for idx=1,#sources do
    local src=sources[idx]
    local sc, sr = src%16, src\16
    local ax=(idx-1)*span
    for ty=0,span-1 do
      for tx=0,span-1 do
        local spr=(sr+ty)*16+(sc+tx)
        mset(ax+tx, atlas_ay+ty, spr)
      end
    end
  end
end

zbuf={}
local door_overlay={}

local function door_slot(ix,iy)
  return (cell(ix,iy)==LOCKED_DOOR_CODE) and 4 or 3
end

function draw_scene()
  local dirx,diry=cos(player.a),sin(player.a)
  local half_fov=fov_turn/2
  local t_half=sin(half_fov)/max(0.0001,cos(half_fov))
  local planex,planey=-diry*t_half, dirx*t_half

  rectfill(0,0,127,63,col_sky)
  rectfill(0,64,127,127,col_floor)

  for i=0,127 do door_overlay[i]=nil end
  local near_clip=0.03

  for x=0,scr_w-1 do
    local camx = x/(scr_w-1)*2-1
    local rdx = dirx + planex*camx
    local rdy = diry + planey*camx

    local mapx,mapy=flr(player.x),flr(player.y)
    local ddx = (rdx==0) and BIG or abs(1/rdx)
    local ddy = (rdy==0) and BIG or abs(1/rdy)
    local stepx,stepy,sdx,sdy
    if rdx<0 then stepx=-1 sdx=(player.x-mapx)*ddx else stepx=1 sdx=(mapx+1-player.x)*ddx end
    if rdy<0 then stepy=-1 sdy=(player.y-mapy)*ddy else stepy=1 sdy=(mapy+1-player.y)*ddy end

    local hit_val=0 local side=0 local guard=0
    while hit_val==0 and guard<256 do
      guard+=1
      if sdx<sdy then sdx+=ddx mapx+=stepx side=0 else sdy+=ddy mapy+=stepy side=1 end
      local ix,iy=mapx+1,mapy+1
      local t=cell(ix,iy)
      if t==DOOR_CODE or t==LOCKED_DOOR_CODE then
        local d=door_at(ix,iy)
        if d then
          local perp=(side==0) and (sdx-ddx) or (sdy-ddy)
          if perp<near_clip then perp=near_clip end
          local wallx
          if side==0 then
            local dx=(mapx - player.x + (1-stepx)/2)
            local hy=player.y + dx*(rdy/(rdx==0 and 0.0001 or rdx))
            wallx=hy - flr(hy)
          else
            local dy=(mapy - player.y + (1-stepy)/2)
            local hx=player.x + dy*(rdx/(rdy==0 and 0.0001 or rdy))
            wallx=hx - flr(hx)
          end
          local tex_x=flr(wallx*tex_w)
          if (side==0 and rdx>0) or (side==1 and rdy<0) then tex_x=tex_w-1-tex_x end
          tex_x=mid(0,tex_x,tex_w-1)

          if d.anim>0 then
            local ubase=(t==LOCKED_DOOR_CODE) and 4 or 3
            local ov=door_overlay[x]
            if (not ov) or (perp<ov.perp) then
              door_overlay[x]={perp=perp, tex_x=tex_x, anim=d.anim, ubase=ubase*span}
            end
          else
            hit_val=t
          end
        end
      elseif t>0 then
        hit_val=t
      end
    end

    local perp=(side==0) and (sdx-ddx) or (sdy-ddy)
    if perp<near_clip then perp=near_clip end
    zbuf[x]=perp

    if hit_val>0 then
      local line_h=flr(scr_h/perp)
      local draw_start=half_h - line_h\2
      local draw_end  =draw_start + line_h - 1

      local wallx
      if side==0 then
        local dx=(mapx - player.x + (1-stepx)/2)
        local hy=player.y + dx*(rdy/(rdx==0 and 0.0001 or rdx))
        wallx=hy - flr(hy)
      else
        local dy=(mapy - player.y + (1-stepy)/2)
        local hx=player.x + dy*(rdx/(rdy==0 and 0.0001 or rdy))
        wallx=hx - flr(hx)
      end
      local tex_x=flr(wallx*tex_w)
      if (side==0 and rdx>0) or (side==1 and rdy<0) then tex_x=tex_w-1-tex_x end
      tex_x=mid(0,tex_x,tex_w-1)

      local tex_sel
      if hit_val==DOOR_CODE then
        tex_sel=3
      elseif hit_val==LOCKED_DOOR_CODE then
        tex_sel=4
      else
        local mxv=max(0,mapx) local myv=max(0,mapy)
        tex_sel = (side==0) and (mxv%3) or (myv%3)
      end

      local ys = clamp(draw_start,0,scr_h-1)
      local ye = clamp(draw_end,0,scr_h-1)
      if ye>ys then
        local step_px = tex_h/line_h
        local tex_pos = (ys - (half_h - line_h/2)) * step_px
        local u=tex_sel*span + tex_x/8
        local v=atlas_ay + tex_pos/8
        tline(x,ys, x,ye, u,v, 0, step_px/8)
      end
    end
  end

  -- overlay opening doors (sliding split)
  for x=0,127 do
    local ov=door_overlay[x]
    if ov then
      local perp=ov.perp
      local line_h=flr(scr_h/perp)
      local draw_start=half_h - line_h\2
      local draw_end=draw_start + line_h - 1
      local midx=tex_w/2
      local gap_half=flr(ov.anim*midx)
      if abs(ov.tex_x - midx) >= gap_half then
        local ys = clamp(draw_start,0,scr_h-1)
        local ye = clamp(draw_end,0,scr_h-1)
        if ye>ys then
          local step_px = tex_h/line_h
          local tex_pos = (ys - (half_h - line_h/2)) * step_px
          local u=ov.ubase + ov.tex_x/8
          local v=atlas_ay + tex_pos/8
          if perp<zbuf[x] then zbuf[x]=perp end
          tline(x,ys, x,ye, u,v, 0, step_px/8)
        end
      end
    end
  end

  draw_key_billboard(dirx,diry,planex,planey)
  draw_exit_billboard(dirx,diry,planex,planey)
  draw_enemies_billboards(dirx,diry,planex,planey)
  draw_projectiles(dirx,diry,planex,planey)
end

-- ===== billboards =====
local function draw_sprite32_billboard(tile_id, depth, screen_x, mode, y_off_frac)
  local s_tx=(tile_id%16)*8
  local s_ty=(tile_id\16)*8
  local src_x,src_y,src_w,src_h=s_tx,s_ty,32,32
  local h=flr(scr_h/depth) local w=h
  if w<1 or h<1 then return end
  local x0=flr(screen_x - w/2)
  local y0
  if mode==1 then y0 = 127 - h
  else
    local off=(y_off_frac or 0)*h
    y0 = flr(half_h - h/2 + off)
  end
  local x1=x0+w-1 if x1<0 or x0>127 then return end
  local xs=max(0,x0) local xe=min(127,x1)
  for x=xs,xe do
    if depth < zbuf[x] then
      local u=flr((x-x0)*src_w/w)
      sspr(src_x+u,src_y,1,src_h, x,y0, 1,h)
    end
  end
end

function draw_exit_billboard(dirx,diry,planex,planey)
  local invdet=1/(planex*diry - dirx*planey)
  local ex,ey=cell_center_world(exit_ix),cell_center_world(exit_iy)
  local rx=ex-player.x local ry=ey-player.y
  local tx=invdet*( diry*rx - dirx*ry)
  local ty=invdet*(-planey*rx + planex*ry)
  if ty>0 then
    local sx=flr((scr_w/2)*(1 + tx/ty))
    draw_sprite32_billboard(EXIT_SPR,ty,sx,0,0.15)
  end
end

function draw_key_billboard(dirx,diry,planex,planey)
  if not key_ent or key_ent.got then return end
  local invdet=1/(planex*diry - dirx*planey)
  local rx=key_ent.x-player.x local ry=key_ent.y-player.y
  local tx=invdet*( diry*rx - dirx*ry)
  local ty=invdet*(-planey*rx + planex*ry)
  if ty>0 then
    local sx=flr((scr_w/2)*(1 + tx/ty))
    draw_sprite32_billboard(KEY_SPR,ty,sx,0,0.10)
  end
end

function draw_enemies_billboards(dirx,diry,planex,planey)
  local invdet=1/(planex*diry - dirx*planey)
  local order={}
  for i=1,#enemies do
    local e=enemies[i]
    local rx=e.x-player.x local ry=e.y-player.y
    local tx=invdet*( diry*rx - dirx*ry)
    local ty=invdet*(-planey*rx + planex*ry)
    add(order,{i=i,ty=ty})
  end
  for a=1,#order do local bi=a for b=a+1,#order do if order[b].ty>order[bi].ty then bi=b end end if bi~=a then local t=order[a] order[a]=order[bi] order[bi]=t end end
  for k=1,#order do
    local e=enemies[order[k].i]
    local rx=e.x-player.x local ry=e.y-player.y
    local tx=invdet*( diry*rx - dirx*ry)
    local ty=invdet*(-planey*rx + planex*ry)
    if ty>0 then
      local sx=flr((scr_w/2)*(1 + tx/ty))
      draw_sprite32_billboard((e.t==0) and SPR_MELEE or SPR_RANGED, ty, sx, 0, 0)
    end
  end
end

function draw_projectiles(dirx,diry,planex,planey)
  local invdet=1/(planex*diry - dirx*planey)
  for pr in all(projectiles) do
    local rx=pr.x-player.x local ry=pr.y-player.y
    local tx=invdet*( diry*rx - dirx*ry)
    local ty=invdet*(-planey*rx + planex*ry)
    if ty>0 then
      local h=flr(scr_h/ty)
      local cx=flr((scr_w/2)*(1 + tx/ty))
      local r=max(1,h\8)
      local cy=half_h
      if cx>=0 and cx<=127 and ty<zbuf[cx] then
        circfill(cx,cy,r,(pr.from==1) and col_proj_pl or col_proj_en)
      end
    end
  end
end
