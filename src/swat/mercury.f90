	  subroutine mercury
	  
!!    ~ ~ ~ PURPOSE ~ ~ ~
!!    this subroutine calculates mercury deposition from atmosphere, leaching, runoff transport
!!    and transformation 

!!    ~ ~ ~ INCOMING VARIABLES ~ ~ ~
!!    name        |units         |definition
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!!    Hg0c(j)     |um/m3          |Atmospheric Hg0 concentration
!!    Hg2c(j)     |um/m3          |Atmospheric Hg(II) concentration
!!    HgMec(j)    |um/m3          |Atmospheric MeHg concentration
!!    sol_cov(:)  |kg/ha          |amount of residue on soil surface
!!    leapyr      |none           |leap year flag
!!                                |0  leap year
!!                                |1  regular year
!!    precipday    |mm H2O        |amount of water reaching soil surface in HRU
!!    subp(:)      |mm H2O        |precipitation for the day in HRU
!!    sol_tmp(:,:) |deg C         |average temperature of soil layer on previous
!!                                |day
!!    hru_ra(:)    |MJ/m^2        |solar radiation for the day in HRU
!!    hru_sub(:)     |none          |subbasin in which HRU is located
!!    sol_st(:,:)      |mm H2O        |amount of water stored in the soil layer
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~

!!    ~ ~ ~ OUTGOING VARIABLES ~ ~ ~
!!    name        |units         |definition
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!!    bactlchlp   |# cfu/m^2     |less persistent bacteria removed from soil
!!                               |surface layer by percolation
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~

!!    ~ ~ ~ LOCAL DEFINITIONS ~ ~ ~
!!    name        |units         |definition
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!!    smf         |              |Fraction of the snow pack melting on a day
!!    bactmx      |
!!    blpq        |# cfu/m^2     |less persistent bacteria in soil solution at
!!                               |beginning of day
!!    conc        |              |concentration of Hg in soil
!!    er          |none          |enrichment ratio
!!    j           |none          |HRU number
!!    wt1         |none          |conversion factor (mg/kg => kg/ha)
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~

!!    ~ ~ ~ SUBROUTINES/FUNCTIONS CALLED ~ ~ ~
!!    Intrinsic: Exp, Min, Max
!!    SWAT: Theta

!!    ~ ~ ~ ~ ~ ~ END SPECIFICATIONS ~ ~ ~ ~ ~ ~

	  use parm

	  integer :: j, nly,ii,iz,k,iji
	  real*8 :: hg_deposit_vel,hgleaf11,hgleaf22
	  real*8, dimension(3) :: hgperc,xx,vhg,conc
	  real*8, dimension(3) :: hg_fall,hglatq,revaphg,gwseephg
	  real*8, dimension(3) :: co,cosurf,hgrchrg1
	  real*8, dimension(2,3) :: yy
	  real*8 :: cecf, sco, dd,hgd,hge,hgm,hgconc 
	  real*8 :: sro, vv,nloss,xy,swc,latqco,gwqco,sedco
	  real*8 :: wt1,ww,frsd,rto,whate,THg_weight,wetsoil_weight
		   
	  j = ihru
	  nly = sol_nly(j)
	  hg_fall = 0.
	  hgperc = 0.
	  cecf = hgpercco
   !----------------------------------------------------------------------------------------------------   
   ! Hg_dry_land(a,b,c) a:dissolved(1)/particulate(2); b:Hg0(1)/Hg2+(2)/MeHg(3); c:ihru
   !----------------------------------------------------------------------------------------------------   
	  
	  !scenario coefficient
	  sco = hg_atmo_co
      
  	  !sedco = 1.5
      latqco = 0.01
      gwqco = 0.01
    
	  
   !! daily Hg input for Jangsung watershed measured data
      if (leapyr>0) then
          dd = iida / 365.
      else
          dd = iida / 366.
      end if
      
	  Hg_dry_land(1,2,j) = sco * (2.88 * dd**3 - 5.13 * dd**2 + 2.29 * dd - 0.0035)  !dissolved Hg2+ dry deposition (mg/ha)

      if (dd<0.5834) then ! curve breaks at the August point
          Hg_dry_land(2,2,j) = sco * (-294.32 * dd**5 + 453.63 * dd**4 - 229.47 * dd**3 + 42.26 * dd**2 - 1.72 * dd + 0.048) !particulate Hg2+ dry deposition (mg/ha)
      else
          Hg_dry_land(2,2,j) = sco * (-1352.6 * dd**5 + 5563.1 * dd**4 - 9098.7 * dd**3 + 7397.6 * dd**2 - 2990.2 * dd + 480.81) !particulate Hg2+ dry deposition (mg/ha)
      end if
      
   !! Hg concentration of air and rainfall  
	  whate = (-18078. * dd**6. + 54256. * dd**5. - 59662. * dd**4. + 28731. * dd**3. - 5120.8 * dd**2. - 141.62 * dd + 128.58) / 1000. !dissolved Hg2+ in rainfall (mg/mm/ha)
      Hg_pcpconc_sub(1,2,hru_sub(j)) = whate

      Hg_airconc(1,1,j) = 1.7  !ng/m3
	  Hg_airconc(2,2,j) = 0.1 !ng/m3
	 
   !! dry deposition 
   !----------------------------------------------------------------------------------------------------   
   !Hg_leaf(a,b,c,d) a:dissolved(1)/particulate(2); b:Hg0(1)/Hg2+(2)/MeHg(3); c:ihru; d:washable(1)/nonwashable(2)
   !----------------------------------------------------------------------------------------------------   
	  
	  ! Canopy storage of nonwashable Hg based on air Hg concentration
	  if (cpnm(idplt(j)).eq."FRSE") then
		  hgleaf11 = (3.* Hg_airconc(1,1,j) + 20.4) * biom_lf(j) * 1e-3 * frac_groundcover(j) *sco      !!! mg/ha converted from ng Hg / g leaf
		  hgleaf22 = (3.* Hg_airconc(2,2,j) + 20.4) * biom_lf(j) * 1e-3 * frac_groundcover(j) *sco   !!! mg/ha converted from ng Hg / g leaf
	  elseif (cpnm(idplt(j)).eq."FRSD") then
		  hgleaf11 = (10.6 * Hg_airconc(1,1,j) + 93.8) * biom_lf(j) * 1e-3 * frac_groundcover(j) *sco      !!! mg/ha converted from ng Hg / g leaf        
		  hgleaf22 = (10.6 * Hg_airconc(2,2,j) + 93.8) * biom_lf(j) * 1e-3 * frac_groundcover(j) *sco      !!! mg/ha converted from ng Hg / g leaf        
	  elseif (cpnm(idplt(j)).eq."FRST") then
		  hgleaf11 = (6.8 * Hg_airconc(1,1,j) + 93.8) *  biom_lf(j) * 1e-3 * frac_groundcover(j) *sco      !!! mg/ha converted from ng Hg / g leaf        
		  hgleaf22 = (6.8 * Hg_airconc(2,2,j) + 93.8) *  biom_lf(j) * 1e-3 * frac_groundcover(j) *sco      !!! mg/ha converted from ng Hg / g leaf        
	  else
		  hgleaf11 = 0
		  hgleaf22 = 0 
		 ! hgleaf11 = Hg_dry_land(1,1,j) * frac_groundcover(j) *sco !mg/ha
		 ! hgleaf22 = Hg_dry_land(2,2,j) * frac_groundcover(j) *sco!mg/ha 
	  endif 
	  
	  if (hgleaf11>Hg_leaf(1,1,j,2)) then !deposition?
		  Hg_leaf(1,1,j,2) = hgleaf11 
	  end if
	  if (hgleaf22>Hg_leaf(2,2,j,2)) then !deposition?
		  Hg_leaf(2,2,j,2) = hgleaf22 
      end if
	  ! Dry deposition of washable Hg on canopy
      Hg_dry_leaf(1,2,j) = Hg_dry_land(1,2,j) * frac_groundcover(j) * sco
      Hg_dry_leaf(2,2,j) = Hg_dry_land(2,2,j) * frac_groundcover(j) * sco
      Hg_leaf(1,2,j,1) = Hg_leaf(1,2,j,1) + Hg_dry_leaf(1,2,j)
      Hg_leaf(2,2,j,1) = Hg_leaf(2,2,j,1) + Hg_dry_leaf(2,2,j)

	  ! Dry deposition of washable Hg on land
      Hg_dry_land(1,2,j) = Hg_dry_land(1,2,j) * (1.- frac_groundcover(j)) * sco
	  Hg_dry_land(2,2,j) = Hg_dry_land(2,2,j) * (1.- frac_groundcover(j)) * sco
	  Hg_dry_total(1,2,j) = Hg_dry_leaf(1,2,j) + Hg_dry_land(1,2,j) 
	  Hg_dry_total(2,2,j) = Hg_dry_leaf(2,2,j) + Hg_dry_land(2,2,j)
	  
   !! wet deposition / wash-off
	  if (subp(j) > 0) then 
		  
	      ! Canopy storage by wet deposition
	      if (canstoday > 0) then !canstoday is estimated based on LAI, not groundcover
		      Hg_wet_leaf_added(1,2,j) = canstoday * Hg_pcpconc_sub(1,2,hru_sub(j)) * sco  ! dissolved Hg2+ wet deposition, mg/ha
		      Hg_leaf(1,2,j,1) = Hg_leaf(1,2,j,1) + Hg_wet_leaf_added(1,2,j)
          end if
          
		  ! direct fall
		  Hg_wet_direct(1,2,j) = subp(j) * (1. - frac_groundcover(j)) * Hg_pcpconc_sub(1,2,hru_sub(j)) * sco  ! dissolved Hg2+ wet deposition,mg/ha
		  
		 ! wash off by rainfall
		  if (snofall<0.01) then
			  ! throughfall
			  Hg_wet_thru(1,2,j) = max(0.,subp(j)-canstoday) * Hg_pcpconc_sub(1,2,hru_sub(j)) * sco ! dissolved Hg2+ throughfall, mg/ha

              !wash off of dissolved Hg2+ from leaves by rainfall
			  rto = min(0., 1. - canstoday / subp(j))
			  if (Hg_leaf(1,2,j,1)>0.) then
			      Hg_wet_thru(1,2,j) = Hg_wet_thru(1,2,j) + Hg_leaf(1,2,j,1) * rto
			      Hg_leaf(1,2,j,1) = Hg_leaf(1,2,j,1) * (1. - rto)
			  endif
			  
			  !wash off of Hg from plant residue to soil
			  Hg_wet_thru(:,:,j) = Hg_wet_thru(:,:,j) + Hg_residue(:,:,j) * (1.- rto)
			  Hg_residue(:,:,j) = Hg_residue(:,:,j) * rto
			 
		  endif
					
		  ! total wet deposition
		  Hg_wet_total(:,:,j) = Hg_wet_thru(:,:,j) + Hg_wet_direct(:,:,j)
 
	  endif
			 
      !! Hg deposited today can go to either snow pack or soil surface 
	  if (sno_hru(j)>0) then
		  Hg_snow(:,:,j) = Hg_snow(:,:,j) + Hg_dry_land(:,:,j) + Hg_wet_total(:,:,j)
	  else
		  Hg_soil_layer(:,:,j,1) = Hg_soil_layer(:,:,j,1) + Hg_dry_land(:,:,j) + Hg_wet_total(:,:,j)
	  endif

	  !! leaf fall goes to residue pool
	  if (leaffall(j)>0.) then
		  frac_leaffall = leaffall(j) / (leaffall(j) + biom_lf(j))
		  Hg_leaf_fall(:,:,j) = (Hg_leaf(:,:,j,1) + Hg_leaf(:,:,j,2)) * frac_leaffall  ! Hg in leaf/litter fall (mg/ha)
		  Hg_leaf(:,:,j,:) = Hg_leaf(:,:,j,:) * (1. - frac_leaffall)
	  end if
	  Hg_residue(:,:,j) = Hg_residue(:,:,j) + Hg_leaf_fall(:,:,j)
	  
   !! residue to soil due to soil/residue mixing
	  frsd = sol_rsd(1,j) / rsdinit(j) !ratio of end-of-day residue and beginning-of-day residue
	  if (frsd<1.) then
		  yy(:,:) = Hg_residue(:,:,j) * (1. - frsd) !amount Hg passing to soil with residue reduction
		  
		  Hg_residue(:,:,j) = Hg_residue(:,:,j) - yy(:,:)
		  Hg_soil_layer(:,:,j,1) = Hg_soil_layer(:,:,j,1) + yy(:,:)
	  end if
			
   !! snow melt moves Hg in snow to soil layer 1
	  if (frac_snomelt>0) then
		  hg_smelt(:,:,j) = Hg_snow(:,:,j) * frac_snomelt
		  Hg_snow(:,:,j) = Hg_snow(:,:,j)  - hg_smelt(:,:,j)
		  Hg_soil_layer(:,:,j,1) = Hg_soil_layer(:,:,j,1) + hg_smelt(:,:,j)
	  end if
	  
   !! Dissolved Hg in soil layers lost in runoff, infiltration, lateral flow, and percolation
	  do jj = 1, sol_nly(j)
   
		!! add Hg leached from layer above
		Hg_soil_layer(1,:,j,jj) = Hg_soil_layer(1,:,j,jj) + hgperc(:)
		do ii=1,3
			if (Hg_soil_layer(1,ii,j,jj) < 1.e-9) Hg_soil_layer(1,ii,j,jj) = 0.0
		end do
		
		!! determine concentration of Hg in mobile water
		sro = 0.; vv = 0.; vhg = 0.; co = 0.
		if (jj == 1) then
		  sro = surfq(j)
		else
		  sro = 0.
		end if
		vv = sol_prk(jj,j) + sro + flat(jj,j) + 1.e-10
		if (ldrain(j) == jj) vv = vv + qtile
		ww = -vv / ((1. - cecf) * sol_ul(jj,j))
		vhg(:) = Hg_soil_layer(1,:,j,jj) * (1. - Exp(ww))
		if (vv > 1.e-10)  co(:) = Max(vhg(:) / vv, 0.)
   
		!! calculate Hg in surface runoff
		cosurf(:) = hgsurqco * co(:)  !hgsurqco is input in *.bsn
		if (jj == 1) then
		  Hg_surq(:,j) = surfq(j) * cosurf(:)
		  do iji=1,3
			  if(surfq(j)>1) Hg_surq(:,j) = Hg_surq(:,j)/surfq(j)**0.5
		  end do
		  Hg_surq(:,j) = Min(Hg_surq(:,j), Hg_soil_layer(1,:,j,jj))
		  !! bmp adjustment
		  !Hg_surq(:,j) = Hg_surq(:,j) * bmp_sn(j)
  		  do k=1,3
            if(Hg_soil_layer(1,k,j,jj)>Hg_surq(k,j))then
		      Hg_soil_layer(1,k,j,jj) = Hg_soil_layer(1,k,j,jj) - Hg_surq(k,j)
            else
              Hg_surq(k,j) = Hg_soil_layer(1,k,j,jj)
              Hg_soil_layer(1,k,j,jj) = 0.
            end if
          end do  
     
  		!! calculate Hg percolation
  		hgperc(:) = co(:) * sol_prk(jj,j)
  		hgperc(:) = Min(hgperc(:), Hg_soil_layer(1,:,j,jj))
  		Hg_soil_layer(1,:,j,jj) = Hg_soil_layer(1,:,j,jj) - hgperc(:)
		endif
   
		!! calculate Hg in lateral flow
		hglatq = 0.
		if (jj == 1) then
		  hglatq(:) = cosurf(:) * flat(jj,j) * latqco
		else
		  hglatq(:) = co(:) * flat(jj,j) * latqco
		end if
		hglatq(:) = Min(hglatq(:), Hg_soil_layer(1,:,j,jj))
   
		!! adjust Hg loss in lateral flow based on the flow distance
		nloss = (2.18 * dis_stream(j) - 8.63) / 100.
		nloss = max(0.,nloss)
		nloss = min(1.,nloss)
		hglatq(:) = (1. - nloss) * hglatq(:) 
   
		do k=1,3
          if(Hg_soil_layer(1,k,j,jj)>hglatq(k))then
              Hg_soil_layer(1,k,j,jj) = Hg_soil_layer(1,k,j,jj) - hglatq(k)
          else
              hglatq(k) = Hg_soil_layer(1,k,j,jj)
              Hg_soil_layer(1,k,j,jj) = 0.
          end if
          Hg_latq(k,j) = Hg_latq(k,j) + hglatq(k)
        end do  
   
		!! calculate Hg percolation
		hgperc(:) = co(:) * sol_prk(jj,j)
		hgperc(:) = Min(hgperc(:), Hg_soil_layer(1,:,j,jj))
   	    do k=1,3
          if(Hg_soil_layer(1,k,j,jj)>hgperc(k))then
            Hg_soil_layer(1,k,j,jj) = Hg_soil_layer(1,k,j,jj) - hgperc(k)
          else
            hgperc(k) = Hg_soil_layer(1,k,j,jj)
            Hg_soil_layer(1,k,j,jj) = 0.
          end if
        end do  
	  end do
   
   !! calculate nitrate leaching from soil profile
	  Hg_perc(:,j) = hgperc(:)
	  
   !! Particulate Hg lost in runoff with soil erosion
      wt1 = sol_bd(1,j) * sol_z(1,j)  !mg/ha (*10^10) soil weight
	  xx(:) = Hg_soil_layer(2,:,j,1) !mg/ha Hg in layer 1
      rto1=min(enratio,0.9)
 	  conc(:) = xx(:) * rto1 / wt1
	  Hg_sedyld(:,j) = 0.1*conc(:) * sedyld(j) / hru_ha(j) !mg/ha
	  
      !! bmp adjustment
	  !Hg_sedyld(:,j) = Hg_sedyld(:,j) * bmp_pn(j)
	  
	  !! update particulate Hg in layer 1
	  Hg_soil_layer(2,:,j,1) = Hg_soil_layer(2,:,j,1) - Hg_sedyld(:,j)
	  
   !! aquifer recharge, return flow, and deep recharge
	  hgrchrg1(:) = Hg_recharge(1,:,j) !from previous day
	  do ii=1,3
		  if (hgrchrg1(ii) < 1.e-6) hgrchrg1(ii) = 0.0
	  end do
	  
	  ! compute Hg aquifer loading from recharge for current day
	  Hg_recharge(:,:,j) = 0.
	  Hg_recharge(1,:,j) = (1.- gw_delaye(j)) * Hg_perc(:,j) + gw_delaye(j) * hgrchrg1(:) !mg/ha
	  Hg_shallst(1,:,j) = Hg_shallst(1,:,j) + Hg_recharge(1,:,j) !mg/ha
	  do ii=1,3
		if (Hg_shallst(1,ii,j) < 1.e-9) Hg_shallst(1,ii,j) = 0.0
	  end do
	  
   !! compute Hg in groundwater return flow for day
	  xx = 0.
	  xy = shallst(j) + gw_q(j) + revapday + gwseep 
	  if (xy > 1.) then
		xx(:) = Hg_shallst(1,:,j) / xy 
	  else
		xx(:) = 0.
	  end if
	  do ii=1,3
		  if (xx(ii) < 1.e-9) xx(ii) = 0.0
	  end do
	  
	  Hg_gwq(:,j) = xx(:) * gw_q(j) * gwqco
   
	  revaphg(:) = amax1(1.e-9,xx(:) * revapday)
	  gwseephg(:) = amax1(1.e-9,xx(:) * gwseep)
   
   !! subtract nitrate losses from the shallow aquifer
	  Hg_shallst(1,:,j) = Hg_shallst(1,:,j) - Hg_gwq(:,j) - revaphg(:) - gwseephg(:)
	  Hg_shallst(1,:,j) = amax1 (0.,  Hg_shallst(1,:,j))
      
   !! Partitioning between particulate and dissolved phases
	  do jj = 1, sol_nly(j)
		  !Hg2+
		  rto = 20000.* (sol_cbn(1,j) / 100.) !Kd based on organic C contents
		  hgt = sum(Hg_soil_layer(:,2,j,jj))
		  Hg_soil_layer(1,2,j,jj) = 1./ (rto + 1.) * hgt   !dissolved Hg2+, mg/ha
		  Hg_soil_layer(2,2,j,jj) = rto / (rto + 1.) * hgt  !particulate Hg2+, mg/ha
		  
		  !MeHg
		  rto = 10000.* (sol_cbn(1,j) / 100.) !Kd based on organic C contents
		  hgt = sum(Hg_soil_layer(:,3,j,jj))
		  Hg_soil_layer(1,3,j,jj) = 1./ (rto + 1.) * hgt   !dissolved MeHg, mg/ha
		  Hg_soil_layer(2,3,j,jj) = rto / (rto + 1.) * hgt  !particulate MeHg, mg/ha
		  
		  !!Hg0
		  !rto = 0. !Kd based on organic C contents
		  !hgt = sum(Hg_soil_layer(:,1,j,jj))
		  !Hg_soil_layer(1,1,j,jj) = 1./ (rto + 1.) * hgt   !dissolved Hg0, mg/ha
		  !Hg_soil_layer(2,1,j,jj) = rto / (rto + 1.) * hgt  !particulate Hg0, mg/ha
		  
		  !partitioning between particulate forms
		  hgt = sum(Hg_soil_layer(2,:,j,jj))
		  Hg_soil_layer(2,1,j,jj) = hgt * 0.01   !Hg2+ vs. CH3Hg+ vs. Hg0 = 98: 1: 1
		  Hg_soil_layer(2,2,j,jj) = hgt * 0.98
		  Hg_soil_layer(2,3,j,jj) = hgt * 0.01
		 
		  Hg_soil(:,:,j) = Hg_soil(:,:,j) + Hg_soil_layer(:,:,j,jj)
	  end do
      
	  !layer 1 and 2
	  THg_weight = sum(Hg_soil_layer(1,:,j,1)) + sum(Hg_soil_layer(2,:,j,1)) !mg/ha
	  THg_weight = THg_weight + sum(Hg_soil_layer(1,:,j,2)) + sum(Hg_soil_layer(2,:,j,2)) !mg/ha
	  wetsoil_weight = sol_bd(1,j) * sol_z(1,j) * (1. - sol_por(1,j)) * exp(-(sol_rockp(1,j) / 100.)) + sol_st(1,j) !t/m3-mm
	  wetsoil_weight = wetsoil_weight + sol_bd(2,j) * sol_z(2,j) * (1. - sol_por(2,j)) * exp(-(sol_rockp(2,j) / 100.)) + sol_st(2,j) !t/m3-mm
      Hg_sc(j) = THg_weight / wetsoil_weight * 0.1  !ug/kg
      
   !! Hg0 flux from soil to atmosphere by reduction/evaporation
      swc = sol_st(1,j) / sol_z(1,j) * 100. !soil moisture content in percent unit
	  !Hgconc = Hg_soil_layer(1,1,j,1) / wetsoil_weight * 0.1  !ug/kg
      Hgconc = Hg_sc(j)
      Hge = max(0., Hgconc * (4.8e-3 + 3.28e-4 * tmpav(j) + 2.21e-3 * hru_ra(j)))    
      Hg0_vola(j) = Hge !ng/m2-hr
          
      Hg0_vola(j) = min(Hg_soil_layer(1,1,j,1), Hg0_vola(j) * .24) !mg/ha-day
      Hg_soil_layer(1,1,j,1) = max(0.,Hg_soil_layer(1,1,j,1) - Hg0_vola(j))

      !total dissolved Hg load in water yield 
      Hg_wyld(:,j) = Hg_surq(:,j) + Hg_latq(:,j) + Hg_gwq(:,j) !mg/ha
      
 !   !----------------------------------------------------------------------
 !         if(j==1) then
 !           write(*,'(i4,6(e10.3,1x))')iida,Hg_soil_layer(1,1,j,1),Hg_soil_layer(1,2,j,1),Hg_soil_layer(1,3,j,1),Hg_soil_layer(2,1,j,1),Hg_soil_layer(2,2,j,1),Hg_soil_layer(2,3,j,1)
 !       endif
 !!----------------------------------------------------------------------
   
!		if(iyr>=1998) write(*,*) iyr, i_mo,j, hru_ha(j)*biom_lf(j)		
	  
	  return
	  end