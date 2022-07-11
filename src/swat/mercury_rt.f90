      subroutine rt_hg
      
!!    ~ ~ ~ PURPOSE ~ ~ ~
!!    this subroutine routes mercury in the main channel

!!    ~ ~ ~ INCOMING VARIABLES ~ ~ ~
!!    name             |units       |definition
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!!    varoute(2,:)     |m^3 H2O     |water flowing into reach on day
!!    varoute(43,:)    |mg       |particulate Hg0 in inflow (mg)
!!    varoute(44,:)    |mg       |particulate Hg2+ in inflow (mg)
!!    varoute(45,:)    |mg       |particulate MeHg in inflow (mg)
!!    varoute(46,:)    |mg       |dissolved Hg0 in inflow (mg)
!!    varoute(47,:)    |mg       |dissolved Hg2+ in inflow (mg)
!!    varoute(48,:)    |mg       |dissolved MeHg in inflow (mg)
!!    inum1            |none        |reach number
!!    inum2            |none        |inflow hydrograph storage location number
!!    rchwtr           |m^3 H2O     |water stored in reach at beginning of day
!!    rnum1            |none        |fraction of overland flow
!!    tmpav(:)         |deg C       |average air temperature on current day
!!    rchstor(:)       |m^3 H2O     |water stored in reach
!!    Hg_chstor(1,1,:,1)    |mg     |dissolved Hg0 in reach water at beginning of day
!!    Hg_chstor(1,2,:,1)    |mg     |dissolved Hg2+ in reach water at beginning of day
!!    Hg_chstor(1,3,:,1)    |mg     |dissolved MeHg in reach water at beginning of day
!!    Hg_chstor(2,1,:,1)    |mg     |particulate Hg0 in reach water at beginning of day
!!    Hg_chstor(2,2,:,1)    |mg     |particulate Hg2+ in reach water at beginning of day
!!    Hg_chstor(2,3,:,1)    |mg     |particulate MeHg in reach water at beginning of day
!!    Hg_chstor(1,1,:,2)    |mg     |dissolved Hg0 in reach sediment at beginning of day
!!    Hg_chstor(1,2,:,2)    |mg     |dissolved Hg2+ in reach sediment at beginning of day
!!    Hg_chstor(1,3,:,2)    |mg     |dissolved MeHg in reach sediment at beginning of day
!!    Hg_chstor(2,1,:,2)    |mg     |particulate Hg0 in reach sediment at beginning of day
!!    Hg_chstor(2,2,:,2)    |mg     |particulate Hg2+ in reach sediment at beginning of day
!!    Hg_chstor(2,3,:,2)    |mg     |particulate MeHg in reach sediment at beginning of day
!!    rchdep         |m             |depth of flow on day
!!    sedst(:)       |metric tons   |amount of sediment in reach waterbody
!!    sedrch         |metric tons   |sediment transported out of channel during time step
!!    sed_deposit    |metric tons   |net deposition of sediment in the channel. Negative value means erosion  
!!    sub_bd(:)      |Mg/m^3        |average bulk density for top 10 mm of soil in subbasin
!!    ch_l2(:)       |km            |length of main channel
!!    ch_w(2,:)      |m             |average width of main channel
!!    sed_wt(jrch)   !tons      !sediment weight in 20cm in the reach
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~

!!    ~ ~ ~ OUTGOING VARIABLES ~ ~ ~
!!    name        |units         |definition
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!!    hbactlp(:)   |# cfu/100mL  |less persistent bacteria in reach/outflow
!!                               |during hour
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~

!!    ~ ~ ~ LOCAL DEFINITIONS ~ ~ ~
!!    name        |units         |definition
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!!    ii          |none          |counter
!!    jrch        |none          |reach number
!!    netwtr      |m^3 H2O       |net amount of water in reach during time step
!!    wtmp        |deg C         |temperature of water in reach
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~

!!    ~ ~ ~ SUBROUTINES/FUNCTIONS CALLED ~ ~ ~
!!    Intrinsic: Exp, Max
!!    SWAT: Theta

!!    ~ ~ ~ ~ ~ ~ END SPECIFICATIONS ~ ~ ~ ~ ~ ~

      use parm
      implicit none

      real, external :: Theta

      integer :: jrch
      real*8 :: wtmp, por_sed,wtrin,sedin
      real*8 :: k_methyl_sed,k_demethyl_sed,k_methyl_wtr,k_demethyl_wtr
      real*8 :: hgdt_wtr,hg_rto
      real*8 :: hgdt_sed1, hgdt_sed2
      real*8 :: dHgp(3),ch_area,dh,msed,rto
      real*8 :: C_hg2d_wtr, C_mehgd_wtr,C_hg2d_sed, C_mehgd_sed 
      real*8 :: hg2flux, mehgflux,kd,sed_erod
      real*8 :: Hg_wtr_tot(3),hgtot1(3),hgtot2(3)
      
      jrch = inum1
      hg_sedcon=0.
	  hg_chstor(1,1,jrch,3)=0.
	  
	  !unknown sources from qhg.dat
	  Hg_chstor(1,1,jrch,1) = Hg_chstor(1,1,jrch,1) + qhg(jrch) / 2. ! dissolved Hg0   mg
	  Hg_chstor(2,2,jrch,1) = Hg_chstor(2,2,jrch,1) + qhg(jrch) / 2. !particulate Hg2+ mg 

		  
   !----------------------------------------------------------------------------------------------------   
   ! Hg_chstor(a,b,c,d) a:dissolved(1)/particulate(2); b:Hg0(1)/Hg2+(2)/MeHg(3); c:jrch; d:water(1)/sedlayer1(2)/sedlayer2(3)
   !----------------------------------------------------------------------------------------------------   
	  
      ! define rate constants
      k_methyl_wtr = 0.0096 ! 1/day
      k_demethyl_wtr = 1.2
      k_methyl_sed = 0.005 
      k_demethyl_sed = 0.5
      por_sed = 0.5
	  
      !area of the channel bed, m^2
      ch_area = ch_w(2,jrch) * ch_l2(jrch) * 1000.
      ! initial water volume
      wtrin = varoute(2,inum2) * (1. - rnum1) + rchstor(jrch) + rtwtr !m3
      sedin = varoute(3,inum2) * (1. - rnum1) + sedst(jrch) + sedrch !ton sed yield

      if (rtwtr > 0. .and. rchdep > 0.) then
          
      !! calculate temperature in stream
      wtmp = 5.0 + 0.75 * tmpav(jrch)
      if (wtmp <= 0.) wtmp = 0.1
      

      !! total Hg mass in reach water at the beginning of the day
      Hg_chstor(1,1,jrch,1) = varoute(46,inum2) + Hg_chstor(1,1,jrch,1) !dissolved Hg0 mg
      Hg_chstor(1,2,jrch,1) = varoute(47,inum2) + Hg_chstor(1,2,jrch,1) !dissolved Hg2+ mg
      Hg_chstor(1,3,jrch,1) = varoute(48,inum2) + Hg_chstor(1,3,jrch,1)!dissolved MeHg mg
      Hg_chstor(2,1,jrch,1) = varoute(43,inum2) + Hg_chstor(2,1,jrch,1)!particulate Hg0 mg
      Hg_chstor(2,2,jrch,1) = varoute(44,inum2) + Hg_chstor(2,2,jrch,1)!particulate Hg2+ mg
      Hg_chstor(2,3,jrch,1) = varoute(45,inum2) + Hg_chstor(2,3,jrch,1)!particulate MeHg mg
      
      ! particulate Hg settling or erosion
      if (sed_deposit>0) then  
          !settling
		  sed_depth1(jrch) = max(0.,sed_depth1(jrch)+dep_z(1))
		  sed_depth2(jrch) = max(0.,sed_depth2(jrch)+dep_z(2))
	      
          dHgp(:) = max(0.,Hg_chstor(2,:,jrch,1) * sed_dep_rto) !mg particulate Hg settling to streambed
          dHgp(:) = min(dHgp(:),Hg_chstor(2,:,jrch,1))
		  hg_rto = dep_z(1)/sum(dep_z(:))
          Hg_chstor(2,:,jrch,1) = max(0.,Hg_chstor(2,:,jrch,1) - dHgp(:)) !mg particulate Hg in water
          Hg_chstor(2,:,jrch,2) = max(0.,Hg_chstor(2,:,jrch,2) + dHgp(:)*hg_rto)   !mg particulate Hg additional sediment layer
		  Hg_chstor(2,:,jrch,3) = max(0.,Hg_chstor(2,:,jrch,3) + dHgp(:)*(1.-hg_rto))   !mg particulate Hg in the channel bed layer
      else
          !eroding
		  if(depch(jrch)==0.)then !channel bed layer eroded
			  
			!additional sediment layer
			sed_depth1(jrch) = max(0.,sed_depth1(jrch)-deg_z(1))
			dHgp(:) = max(0.,Hg_chstor(2,:,jrch,2))  !mg particulate Hg resuspended.  
            dHgp(:) = min(dHgp(:),Hg_chstor(2,:,jrch,2))
			Hg_chstor(2,:,jrch,2)=0. !all Hg in the additonal sediment layer is resuspended
            Hg_chstor(2,:,jrch,1) = max(0.,Hg_chstor(2,:,jrch,1) + dHgp(:)) !mg particulate Hg in water
			
			!channel bed layer
			sed_depth2(jrch) = max(0.,sed_depth2(jrch)-deg_z(2))
			if(sed_depth2(jrch)+deg_z(2)>0) then
				hg_rto = deg_z(2) / (sed_depth2(jrch)+deg_z(2)) !ratio of sediment resuspended in the channel bed layer
				dHgp(:) = max(0.,Hg_chstor(2,:,jrch,3)*hg_rto)  !mg particulate Hg resuspended.  
				dHgp(:) = min(dHgp(:),Hg_chstor(2,:,jrch,3))
				Hg_chstor(2,:,jrch,1) = max(0.,Hg_chstor(2,:,jrch,1) + dHgp(:)) !mg particulate Hg in water
				Hg_chstor(2,:,jrch,3) = max(0.,Hg_chstor(2,:,jrch,3) - dHgp(:)*(1.-hg_rto))   !mg particulate Hg channel bed layer
			endif
		  else
			sed_depth1(jrch) = max(0.,sed_depth1(jrch)-deg_z(1))
			if(sed_depth1(jrch)>0) then
				hg_rto = deg_z(1)/(sed_depth1(jrch)+deg_z(1))
			else
				hg_rto = 1.
			endif
			dHgp(:) = max(0.,Hg_chstor(2,:,jrch,2)*hg_rto)  !mg particulate Hg resuspended from the additional sediment layer.  
            Hg_chstor(2,:,jrch,1) = max(0.,Hg_chstor(2,:,jrch,1) + dHgp(:)) !mg particulate Hg in water
			Hg_chstor(2,:,jrch,2) = max(0.,Hg_chstor(2,:,jrch,2) - dHgp(:)) !all Hg in the additonal sediment layer is resuspended

			sed_depth2(jrch) = max(0.,sed_depth2(jrch)-deg_z(2))
			if(sed_depth2(jrch)>0) then
				hg_rto = deg_z(2)/(sed_depth2(jrch)+deg_z(2))
			else
				hg_rto = 1.
			endif
			dHgp(:) = max(0.,Hg_chstor(2,:,jrch,3)*hg_rto)  !mg particulate Hg resuspended from the channel bed  
            Hg_chstor(2,:,jrch,1) = max(0.,Hg_chstor(2,:,jrch,1) + dHgp(:)) !mg particulate Hg in water
			Hg_chstor(2,:,jrch,3) = max(0.,Hg_chstor(2,:,jrch,3) - dHgp(:)) !all Hg in the channel bed is resuspended
	      endif			  

      endif
      hgdt_wtr = sum(Hg_chstor(1,:,jrch,1)) !mg dissolved THg (=Hg0+Hg2+MeHg) in water
      hgdt_sed1 = sum(Hg_chstor(1,:,jrch,2)) !mg dissolved THg (=Hg0+Hg2+MeHg) in additional sediment layer
      hgdt_sed2 = sum(Hg_chstor(1,:,jrch,3)) !mg dissolved THg (=Hg0+Hg2+MeHg) in channel bed layer

 	!if(jrch==11) write(*,*)iida, sed_depth1(jrch),sed_depth2(jrch)
    
      ! partitioning of dissolved hg species in water 
      rto = 1./ (k_methyl_wtr / k_demethyl_wtr + 1.)
      rto = int(rto*1000.)
      rto = rto/1000.
      Hg_chstor(1,2,jrch,1) = hgdt_wtr * rto            !mg
      Hg_chstor(1,3,jrch,1) = hgdt_wtr - hgdt_wtr * rto
      Hg_chstor(1,1,jrch,1) = 0.
      

      ! partitioning of hg in water between particulate and dissolved form
      if(sedin>0 .and.wtrin>0)then
		  kd = 4.*10**5 !Hg partitioning between solid and dissolved (L/KG)
		  Hg_wtr_tot(:) = Hg_chstor(1,:,jrch,1) + Hg_chstor(2,:,jrch,1)
		  rto = int(sedin * kd / (sedin * kd + wtrin)*1000.)
		  rto = rto/1000.
		  Hg_chstor(2,:,jrch,1) = rto * Hg_wtr_tot(:)
		  Hg_chstor(1,:,jrch,1) = Hg_wtr_tot(:) -  Hg_chstor(2,:,jrch,1)
	  endif

	  ! partitioning dissolved hg species in sediment for equilibrium condition
      Hg_chstor(1,2,jrch,2) = hgdt_sed1 / (k_methyl_sed / k_demethyl_sed + 1.)   !mg
      Hg_chstor(1,3,jrch,2) = hgdt_sed1 / (k_demethyl_sed / k_methyl_sed + 1.)
      Hg_chstor(1,1,jrch,2) = hgdt_sed1 - (Hg_chstor(1,2,jrch,2) +  Hg_chstor(1,3,jrch,2))
      Hg_chstor(1,2,jrch,3) = hgdt_sed2 / (k_methyl_sed / k_demethyl_sed + 1.)
      Hg_chstor(1,3,jrch,3) = hgdt_sed2 / (k_demethyl_sed / k_methyl_sed + 1.)
      Hg_chstor(1,1,jrch,3) = hgdt_sed2 - (Hg_chstor(1,2,jrch,3) +  Hg_chstor(1,3,jrch,3))
      if (Hg_chstor(1,1,jrch,2)<0) Hg_chstor(1,1,jrch,2) = 0.
      if (Hg_chstor(1,1,jrch,3)<0) Hg_chstor(1,1,jrch,3) = 0.
     
      !!! partitioning of hg in sediment between particulate and dissolved form
      kd = 8.9e3 ! (L/KG) -> Hg0, Hg2+
      hgtot1(:) = Hg_chstor(1,:,jrch,2) + Hg_chstor(2,:,jrch,2)
      hgtot2(:) = Hg_chstor(1,:,jrch,3) + Hg_chstor(2,:,jrch,3)
      if(sed_depth1(jrch)>0.0001)then 
		  conc_hgpsed1(1:2) = hgtot1(1:2) * kd / (10.* ch_area * sed_depth1(jrch)*100. * (sub_bd(jrch) * kd + por_sed)) !mg/kg
		  conc_hgpsed1(3) = hgtot1(3) * 2.9e2 / (10.* ch_area * sed_depth1(jrch)*100. * (sub_bd(jrch) * 2.9e2 + por_sed)) !mg/kg
	  else
		  conc_hgpsed1(1:2)=0.
		  conc_hgpsed1(3) =0.
	  endif
      
      if(sed_depth2(jrch)>0.0001)then 
		  conc_hgpsed2(1:2) = hgtot2(1:2) * kd / (10.* ch_area * sed_depth2(jrch)*100. * (sub_bd(jrch) * kd + por_sed)) !mg/kg
		  conc_hgpsed2(3) = hgtot2(3) * 2.9e2 / (10.* ch_area * sed_depth2(jrch)*100. * (sub_bd(jrch) * 2.9e2 + por_sed)) !mg/kg
	  else
		  conc_hgpsed2(1:2)=0.
		  conc_hgpsed2(3)=0.
	  endif
      
      conc_hgdsed1(1:2) = conc_hgpsed1(1:2) / kd !mg/L
      conc_hgdsed2(1:2) = conc_hgpsed2(1:2) / kd !mg/L
     
      conc_hgdsed1(3) = conc_hgpsed1(3) / kd !mg/L
      conc_hgdsed2(3) = conc_hgpsed2(3) / kd !mg/L
	  
      Hg_chstor(1,:,jrch,2) = 10. * por_sed * conc_hgdsed1(:) * ch_area * sed_depth1(jrch)*100.  !mg
      Hg_chstor(2,:,jrch,2) = hgtot1(:) - Hg_chstor(1,:,jrch,2)  !mg

      Hg_chstor(1,:,jrch,3) = 10. * por_sed * conc_hgdsed2(:) * ch_area * sed_depth2(jrch)*100.  !mg
      Hg_chstor(2,:,jrch,3) = hgtot2(:) - Hg_chstor(1,:,jrch,3) !mg

      ! flux of dissolved Hg between water and sediment layer 1
      C_hg2d_wtr = Hg_chstor(1,2,jrch,1) / (rchdep * 100.)  ! dissolved Hg2+ concentration in water mg/cm
      C_mehgd_wtr = Hg_chstor(1,3,jrch,1) / (rchdep * 100.)  ! dissolved MeHg concentration in water mg/cm
      C_hg2d_sed = Hg_chstor(1,2,jrch,2) / 5.  ! dissolved Hg2+ concentration in sediment pore mg/cm  diffusion layer thickness=0.5cm 
      C_mehgd_sed = Hg_chstor(1,3,jrch,2) / 5. ! dissolved MeHg concentration in sediment pore mg/cm 

      hg2flux = -0.226  * (C_hg2d_wtr - C_hg2d_sed)*0.82/0.6       !dissolved Hg2+ from sediment to water, mg/day
      mehgflux = -0.298  * (C_mehgd_wtr - C_mehgd_sed)*0.82/0.6   !dissolved MeHg from sediment to water, mg/day
       
      !tmp1=sum(Hg_chstor(1,2:3,jrch,1:2))
      Hg_chstor(1,2,jrch,1) = max(0.,Hg_chstor(1,2,jrch,1) + hg2flux)  !mg
      Hg_chstor(1,3,jrch,1) = max(0.,Hg_chstor(1,3,jrch,1) + mehgflux) 
      Hg_chstor(1,2,jrch,2) = max(0.,Hg_chstor(1,2,jrch,2) - hg2flux) 
      Hg_chstor(1,3,jrch,2) = max(0.,Hg_chstor(1,3,jrch,2) - mehgflux) 
       !tmp2=sum(Hg_chstor(1,2:3,jrch,1:2))
      
      ! Hg discharge in outflow
      varoute(46,ihout) = max(0.,Hg_chstor(1,1,jrch,1) * (1. - rchstor(jrch) / wtrin)) !dissolved Hg0 mg
      varoute(47,ihout) = max(0.,Hg_chstor(1,2,jrch,1) * (1. - rchstor(jrch) / wtrin)) !dissolved Hg2+ mg
      varoute(48,ihout) = max(0.,Hg_chstor(1,3,jrch,1) * (1. - rchstor(jrch) / wtrin)) !dissolved MeHg mg
      varoute(43,ihout) = max(0.,Hg_chstor(2,1,jrch,1) * (1. - rchstor(jrch) / wtrin)) !particulate Hg0 mg
      varoute(44,ihout) = max(0.,Hg_chstor(2,2,jrch,1) * (1. - rchstor(jrch) / wtrin)) !particulate Hg2+ mg
      varoute(45,ihout) = max(0.,Hg_chstor(2,3,jrch,1) * (1. - rchstor(jrch) / wtrin)) !particulate MeHg mg
      
		!Hg_chstor(1,1,jrch,3) = sum(Hg_chstor(:,:,jrch,1))/wtrin !THg in water ppb
	  hg_chstor(1,1,jrch,3) = sum(varoute(43:48,ihout))/rtwtr !THg in water ppb
	  
      Hg_chstor(1,1,jrch,1) = max(0.,Hg_chstor(1,1,jrch,1) - varoute(46,ihout)) !dissolved Hg0 mg
      Hg_chstor(1,2,jrch,1) = max(0.,Hg_chstor(1,2,jrch,1) - varoute(47,ihout)) !dissolved Hg2+ mg
      Hg_chstor(1,3,jrch,1) = max(0.,Hg_chstor(1,3,jrch,1) - varoute(48,ihout)) !dissolved MeHg mg
      Hg_chstor(2,1,jrch,1) = max(0.,Hg_chstor(2,1,jrch,1) - varoute(43,ihout)) !particulate Hg0 mg
      Hg_chstor(2,2,jrch,1) = max(0.,Hg_chstor(2,2,jrch,1) - varoute(44,ihout)) !particulate Hg2+ mg
      Hg_chstor(2,3,jrch,1) = max(0.,Hg_chstor(2,3,jrch,1) - varoute(45,ihout)) !particulate MeHg mg
  
      if(Hg_chstor(1,1,jrch,1)<0) Hg_chstor(1,1,jrch,1) = 0.
      if(Hg_chstor(1,2,jrch,1)<0) Hg_chstor(1,1,jrch,1) = 0.
      if(Hg_chstor(1,3,jrch,1)<0) Hg_chstor(1,1,jrch,1) = 0.
      if(Hg_chstor(2,1,jrch,1)<0) Hg_chstor(1,1,jrch,1) = 0.
      if(Hg_chstor(2,2,jrch,1)<0) Hg_chstor(1,1,jrch,1) = 0.
      if(Hg_chstor(2,3,jrch,1)<0) Hg_chstor(1,1,jrch,1) = 0.
      
      else
      varoute(43:48,ihout) = 0.
      Hg_chstor(:,:,jrch,1) = 0.

	  !!! partitioning of hg in sediment between particulate and dissolved form
      kd = 8.9e3 ! (L/KG) -> Hg0, Hg2+
      hgtot1(:) = Hg_chstor(1,:,jrch,2) + Hg_chstor(2,:,jrch,2)
      hgtot2(:) = Hg_chstor(1,:,jrch,3) + Hg_chstor(2,:,jrch,3)
      if(sed_depth1(jrch)>0)then 
		  conc_hgpsed1(1:2) = hgtot1(1:2) * kd / (10.* ch_area * sed_depth1(jrch)*100. * (sub_bd(jrch) * kd + por_sed)) !mg/kg
		  conc_hgpsed1(3) = hgtot1(3) * 2.9e2 / (10.* ch_area * sed_depth1(jrch)*100. * (sub_bd(jrch) * kd + por_sed)) !mg/kg
	  else
		  conc_hgpsed1(1:2)=0.
		  conc_hgpsed1(3) =0.
	  endif
      if(sed_depth2(jrch)>0)then 
		  conc_hgpsed2(1:2) = hgtot2(1:2) * kd / (10.* ch_area * sed_depth2(jrch)*100. * (sub_bd(jrch) * kd + por_sed)) !mg/kg
		  conc_hgpsed2(3) = hgtot2(3) * 2.9e2 / (10.* ch_area * sed_depth2(jrch)*100. * (sub_bd(jrch) * kd + por_sed)) !mg/kg
	  else
		  conc_hgpsed2(1:2)=0.
		  conc_hgpsed2(3)=0.
	  endif
      conc_hgdsed1(1:2) = conc_hgpsed1(1:2) / kd !mg/L
      conc_hgdsed2(1:2) = conc_hgpsed2(1:2) / kd !mg/L
      conc_hgdsed1(3) = conc_hgpsed1(3) / 2.9e2 !mg/L
	  
      Hg_chstor(1,:,jrch,2) = 10. * por_sed * conc_hgdsed1(:) * ch_area * sed_depth1(jrch)*100.  !mg
      Hg_chstor(2,:,jrch,2) = hgtot1(:) - Hg_chstor(1,:,jrch,2)  !mg
      Hg_chstor(1,:,jrch,3) = 10. * por_sed * conc_hgdsed2(:) * ch_area * sed_depth2(jrch)*100.  !mg
      Hg_chstor(2,:,jrch,3) = hgtot2(:) - Hg_chstor(1,:,jrch,3)  !mg
      endif
	  hg_sedcon = (sum(hgtot1(:))+sum(hgtot2(:)))/(ch_area*sub_bd(jrch)*(sed_depth1(jrch)+sed_depth2(jrch))*1000.)
	  !if(jrch==1)write(*,*)iida,sed_depth1(jrch),sed_depth2(jrch),sed_deposit,hg_sedcon
      return
      end