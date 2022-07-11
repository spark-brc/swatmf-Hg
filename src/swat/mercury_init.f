	  subroutine hg_init 

!!    ~ ~ ~ PURPOSE ~ ~ ~
!!    This subroutine computes variables related to the watershed hydrology:
!!    the time of concentration for the subbasins, lagged surface runoff,
!!    the coefficient for the peak runoff rate equation, and lateral flow travel
!!    time.


	  use parm

	  integer:: jres,jj
        real*8 :: hgco,sed_por_vol,bulk_density,sed_sol,ch_a
	  real*8:: conc_hg(nrch)
        
	  bulk_density = 1.54 !sum(sub_bd(:)) / msub
	  
	  !hgco = 2.3
	  !hgpercco = 0.12
	  hgco = 2.5 * hg_sol_co
	  !hgpercco = 0.06  
		por_sed = 0.5
   !----------------------------------------------------------------------------------------------------   
   ! Hg_soil_layer(a,b,c,d) a:dissolved(1)/particulate(2); b:Hg0(1)/Hg2+(2)/MeHg(3); c:ihru; d:layer
   !----------------------------------------------------------------------------------------------------   
	 !!Initial soil contents of Mercury
	  Hg_soil_layer(1,1,:,:) = 90.*hgco
	  Hg_soil_layer(2,2,:,1) = 8549.1*hgco
	  Hg_soil_layer(1,2,:,:) = 0.9*hgco
	  Hg_soil_layer(2,3,:,1) = 359.9*hgco
	  Hg_soil_layer(1,1,:,:) = 0
        
	 conc_hg=(/1.2,1.7,2.0,1.9,0.9,1.7,0.9,1.2,1.1,1.1,1.1,1.1,1.2,0.,1.0/) !distribute Hg soil content among subbasins based on sample values
       do j=1,nhru
		 Hg_soil_layer(:,:,j,:) = conc_hg(hru_sub(j))* Hg_soil_layer(:,:,j,:)
	 end do
	             
   !----------------------------------------------------------------------------------------------------   
   ! Hg_chstor(a,b,c,d) a:dissolved(1)/particulate(2); b:Hg0(1)/Hg2+(2)/MeHg(3); c:jrch; d:water(1)/sedlayer1(2)/sedlayer2(3)
   !----------------------------------------------------------------------------------------------------   
	conc_hg=(/20,20,22,23,38,57,40,43,65,77,95,73,79,60,50/)
	conc_hg=conc_hg*eros_spl
	do jrch=1,nrch
		
	ch_a=ch_w(2,jrch)*ch_l2(jrch)*1000. !m2
      sed_depth2(jrch)=1. !m
	Hg_chstor(2,2,jrch,3)=1000.*1.3*conc_hg(jrch)*ch_a
     &	*sed_depth2(jrch)  !channel bed layer Hg, mg, 1.m thickness
	sed_wt(jrch)= 1.3 * ch_a * sed_depth2(jrch) !sediment weight in 1.m, tons

	read(7746003,*) jj,qhg(jrch)

	enddo
	
	close(7746003)
	  
        !reservoir initial condition based on Jangsung Lake sample data
        do jres=1, nres
            ressa = res_psa(jres)             !reservoir water surface area, ha
            if (res_vol(jres)>0.1.and.ressa>0.001) then
                resdepth = res_vol(jres) / (ressa*1e4) !water depth, m 
            else
                resdepth = 0.0001
            endif
            
            Hg_resstor(1,2,jres,1)=0.92*1e-3*res_vol(jres) !dissolved Hg2+ mg
            Hg_resstor(1,3,jres,1)=0.04*1e-3*res_vol(jres) !dissolved MeHg mg
            Hg_resstor(2,2,jres,1)=0.35*1e-3*res_vol(jres) !particulate Hg2+ mg
            Hg_resstor(2,3,jres,1)=0.15*1e-3*res_vol(jres)  !particulate MeHg mg      

            !sediment layer1
            sed_por_vol = ressa*1e4 * 0.05 * 0.82 !m3 pore volume 5cm layer thickness, porosity=0.6
            sed_sol = bulk_density * 0.18 * ressa*1e4 * 0.05  !tons soil
            Hg_resstor(1,2,jres,2) = 14.2*1e-3*sed_por_vol !dissolved Hg2+ in 5cm sediment layer mg
            Hg_resstor(1,3,jres,2) = 2.45*1e-3*sed_por_vol !dissolved MeHg in 5cm sediment layer mg
            Hg_resstor(2,2,jres,2) = 160.8 * sed_sol  !particulate Hg2+ in 5cm sediment layer mg
            Hg_resstor(2,3,jres,2) = 1.2 * sed_sol  !particulate MeHg in 5cm sediment layer mg        
            Hg_resstor(1,2,jres,3) = Hg_resstor(1,2,jres,2)
            Hg_resstor(1,3,jres,3) = Hg_resstor(1,3,jres,2)
            Hg_resstor(2,2,jres,3) = Hg_resstor(2,2,jres,2)
            Hg_resstor(2,3,jres,3) = Hg_resstor(2,3,jres,2)  
	  end do
        return
	  end