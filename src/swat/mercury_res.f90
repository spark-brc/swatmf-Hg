      subroutine mercury_res
      
!!    ~ ~ ~ PURPOSE ~ ~ ~
!!    this subroutine models Mercury in reservoirs

!!    ~ ~ ~ INCOMING VARIABLES ~ ~ ~
!!    name             |units       |definition
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!!    inum1                  |none          |reservoir number
!!    inum2                  |none          |inflow hydrograph storage location number
!!    reswtr                 |m^3 H2O       |initial reservoir volume
!!    tmpav(:)               |deg C         |average air temperature on current day
!!    varoute(2,:)           |m^3 H2O       |water flowing into reservoir on day
!!    varoute(43,:)          |mg            |particulate Hg0 in inflow (mg)
!!    varoute(44,:)          |mg            |particulate Hg2+ in inflow (mg)
!!    varoute(45,:)          |mg            |particulate MeHg in inflow (mg)
!!    varoute(46,:)          |mg            |dissolved Hg0 in inflow (mg)
!!    varoute(47,:)          |mg            |dissolved Hg2+ in inflow (mg)
!!    varoute(48,:)          |mg            |dissolved MeHg in inflow (mg)
!!    Hg_chstor(1,1,:,1)     |mg            |dissolved Hg0 in reach water at beginning of day
!!    Hg_resstor(1,2,:,1)    |mg            |dissolved Hg2+ in reach water at beginning of day
!!    Hg_resstor(1,3,:,1)    |mg            |dissolved MeHg in reach water at beginning of day
!!    Hg_resstor(2,1,:,1)    |mg            |particulate Hg0 in reach water at beginning of day
!!    Hg_resstor(2,2,:,1)    |mg            |particulate Hg2+ in reach water at beginning of day
!!    Hg_resstor(2,3,:,1)    |mg            |particulate MeHg in reach water at beginning of day
!!    Hg_resstor(1,1,:,2)    |mg            |dissolved Hg0 in reach sediment at beginning of day
!!    Hg_resstor(1,2,:,2)    |mg            |dissolved Hg2+ in reach sediment at beginning of day
!!    Hg_resstor(1,3,:,2)    |mg            |dissolved MeHg in reach sediment at beginning of day
!!    Hg_resstor(2,1,:,2)    |mg            |particulate Hg0 in reach sediment at beginning of day
!!    Hg_resstor(2,2,:,2)    |mg            |particulate Hg2+ in reach sediment at beginning of day
!!    Hg_resstor(2,3,:,2)    |mg            |particulate MeHg in reach sediment at beginning of day
!!    res_sed(:)             |ton/m^3       |amount of sediment in reservoir at the end of day
!!    res_sed_init(:)        |ton/m^3       |sediment concentration in reservoir at the beginning of day
!!    res_vol(:)             |m^3 H2O       |reservoir volume
!!    resev                  |m^3 H2O       |evaporation from reservoir on day
!!    resflwo                |m^3 H2O       |water leaving reservoir on day
!!    respcp                 |m^3 H2O       |precipitation on reservoir for day
!!    ressa                  |ha            |surface area of reservoir on day
!!    ressep                 |m^3 H2O       |seepage from reservoir on day
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~

!!    ~ ~ ~ OUTGOING VARIABLES ~ ~ ~
!!    name        |units         |definition
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~

!!    ~ ~ ~ LOCAL DEFINITIONS ~ ~ ~
!!    name        |units         |definition
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!!    jres        |none          |reservoir number

!!    ~ ~ ~ SUBROUTINES/FUNCTIONS CALLED ~ ~ ~
!!    Intrinsic: Exp, Max

!!    ~ ~ ~ ~ ~ ~ END SPECIFICATIONS ~ ~ ~ ~ ~ ~

      use parm
      implicit none

      integer :: jres

      real*8 :: por_sed,sed_settle,bulk_density
      real*8 :: k_methyl_sed,k_demethyl_sed,k_methyl_wtr,k_demethyl_wtr
      real*8 :: hgdt_wtr, rto,thgdsed,thgpsed,mhgdsed,mhgpsed
      real*8 :: hgdt_sed1, hgdt_sed2
      real*8 :: dHgp(3),dh,msed,settle_rto
      real*8 :: C_hg2d_wtr, C_mehgd_wtr,C_hg2d_sed, C_mehgd_sed 
      real*8 :: hg2flux, mehgflux, sedf, frto,resdepth,kd,sed_erod
      real*8 :: conc_res_hgpsed1(3),conc_res_hgpsed2(3),conc_res_hgdsed1(3),conc_res_hgdsed2(3),Hg_wtr_tot(3),hgtot1(3),hgtot2(3)
            
      jres = inum1
      if (res_vol(jres) < 1.) then
        Hg_resstor(:,:,jres,1) = 0.
        return
      end if

      resdepth = 0.0001
      if (res_vol(jres)>0.1.and.ressa>0.001) resdepth = res_vol(jres) / (ressa*1e4) !water depth, m 

     
   !----------------------------------------------------------------------------------------------------------------------------   
   ! Hg_resstor(a,b,c,d) a:dissolved(1)/particulate(2); b:Hg0(1)/Hg2+(2)/MeHg(3); c:jrres; d:water(1)/sedlayer1(2)/sedlayer2(3)
   !----------------------------------------------------------------------------------------------------------------------------   
          
      ! define rate constants
      k_methyl_wtr = 0.0096 ! 1/day
      k_demethyl_wtr = 0.18 !1.2
      k_methyl_sed = 0.005 
      k_demethyl_sed = 0.5
      por_sed = 0.82
      bulk_density = 1.54 !sum(sub_bd(:)) / msub

      sed_settle =  (res_sed_init(jres) - res_sed(jres)) * res_vol(jres)  !tons sediment settled     
      settle_rto = min(1.,sed_settle / (res_sed_init(jres)* res_vol(jres))) !ratio of sediment settling and suspended sediment
      
      !! total Hg mass in reservoir water at the beginning of the day
      Hg_resstor(2,1,jres,1) = varoute(43,inum2) + Hg_resstor(2,1,jres,1) !particulate Hg0 mg
      Hg_resstor(2,2,jres,1) = varoute(44,inum2) + Hg_resstor(2,2,jres,1) !particulate Hg2+ mg
      Hg_resstor(2,3,jres,1) = varoute(45,inum2) + Hg_resstor(2,3,jres,1) !particulate MeHg mg
      Hg_resstor(1,1,jres,1) = varoute(46,inum2) + Hg_resstor(1,1,jres,1) !dissolved Hg0 mg
      Hg_resstor(1,2,jres,1) = varoute(47,inum2) + Hg_resstor(1,2,jres,1) !dissolved Hg2+ mg
      Hg_resstor(1,3,jres,1) = varoute(48,inum2) + Hg_resstor(1,3,jres,1) !dissolved MeHg mg
      
      ! particulate Hg settling or erosion
      if (settle_rto>0) then
          !settling
          dHgp(:) = max(0.,Hg_resstor(2,:,jres,1) * settle_rto*0.1) !mg particulate Hg settling to streambed
          dHgp(:) = min(dHgp(:),Hg_resstor(2,:,jres,1)) !Hg settling should not be greater than Hg in water
          dh = sed_settle / (bulk_density * por_sed * ressa*10000.) * 100. !cm
          Hg_resstor(2,:,jres,1) = max(0.,Hg_resstor(2,:,jres,1) - dHgp(:)) !mg particulate Hg in water
          Hg_resstor(2,:,jres,3) = max(0.,Hg_resstor(2,:,jres,3) + Hg_resstor(2,:,jres,2) * dh / 5.)   !mg particulate Hg sediment layer2
          Hg_resstor(2,:,jres,2) = max(0.,Hg_resstor(2,:,jres,2) * (1. - dh / 5.) + dHgp(:))   !mg particulate Hg sediment layer1
          !write(*,*) iyr, iida, sum(dHgp(:))
      else
          !eroding
          sed_erod = max(0.,-sed_deposit) !erosion rate, tons
          msed = bulk_density * ressa * 5. * 100. !metric tons sediment in layer 1    
          dHgp(:) = max(0.,Hg_resstor(2,:,jres,2) * sed_erod / msed) !mg particulate Hg resuspended. 
          dHgp(:) = min(dHgp(:),Hg_resstor(2,:,jres,2)) !Hg resuspension should not be greater than Hg in sed layer1
          dh = sed_erod / (sub_bd(jres) * por_sed * ressa*10000.) * 100. !cm depth eroded
          Hg_resstor(2,:,jres,1) = max(0.,Hg_resstor(2,:,jres,1) + dHgp(:)) !mg particulate Hg in water
          Hg_resstor(2,:,jres,2) = max(0.,Hg_resstor(2,:,jres,2) - dHgp(:) + Hg_resstor(2,:,jres,3) * dh / 5.)  !mg particulate Hg sediment layer1
          Hg_resstor(2,:,jres,3) = max(0.,Hg_resstor(2,:,jres,3) - Hg_resstor(2,:,jres,3) * dh / 15.)   !mg particulate Hg sediment layer2
           !write(*,*) iyr, iida, -sum(dHgp(:))
     endif
      
      hgdt_wtr = sum(Hg_resstor(1,:,jres,1)) !mg dissolved THg (=Hg0+Hg2+MeHg) in water
      hgdt_sed1 = sum(Hg_resstor(1,:,jres,2)) !mg dissolved THg (=Hg0+Hg2+MeHg) in sediment layer1
      hgdt_sed2 = sum(Hg_resstor(1,:,jres,3)) !mg dissolved THg (=Hg0+Hg2+MeHg) in sediment layer2
      
      ! partitioning dissolved hg species in water for equilibrium condition
      rto = 1./ (k_methyl_wtr / k_demethyl_wtr + 1.)
      rto = int(rto*1000.)
      rto = rto/1000.
      Hg_resstor(1,2,jres,1) = hgdt_wtr * rto     !mg
      Hg_resstor(1,3,jres,1) = hgdt_wtr - hgdt_wtr * rto
      Hg_resstor(1,1,jres,1) = 0.
      
      ! partitioning of hg in water between particulate and dissolved form
      kd = 4.*10**5 !Hg partitioning between solid and dissolved (L/KG)
      sedf = res_sed(jres) *  res_vol(jres) !tons sediment in reservoir  
      Hg_wtr_tot(:) = Hg_resstor(1,:,jres,1) + Hg_resstor(2,:,jres,1)
      rto = int(sedf * kd / (sedf * kd + res_vol(jres))*1000.)
      rto = rto/1000.
      Hg_resstor(2,:,jres,1) = rto * Hg_wtr_tot(:)
      Hg_resstor(1,:,jres,1) = Hg_wtr_tot(:) - Hg_resstor(2,:,jres,1)
      
      ! partitioning dissolved hg species in sediment for equilibrium condition
      Hg_resstor(1,2,jres,2) = hgdt_sed1 / (k_methyl_sed / k_demethyl_sed + 1.)   !mg
      Hg_resstor(1,3,jres,2) = hgdt_sed1 / (k_demethyl_sed / k_methyl_sed + 1.)
      Hg_resstor(1,1,jres,2) = hgdt_sed1 - (Hg_resstor(1,2,jres,2) +  Hg_resstor(1,3,jres,2))
      Hg_resstor(1,2,jres,3) = hgdt_sed2 / (k_methyl_sed / k_demethyl_sed + 1.)
      Hg_resstor(1,3,jres,3) = hgdt_sed2 / (k_demethyl_sed / k_methyl_sed + 1.)
      Hg_resstor(1,1,jres,3) = hgdt_sed2 - (Hg_resstor(1,2,jres,3) +  Hg_resstor(1,3,jres,3))
      if (Hg_resstor(1,1,jres,2)<0) Hg_resstor(1,1,jres,2) = 0.
      if (Hg_resstor(1,1,jres,3)<0) Hg_resstor(1,1,jres,3) = 0.
      
      ! partitioning of hg in sediment between particulate and dissolved form
      kd = 8.9e3 ! (L/KG) -> Hg0, Hg2+
      hgtot1(:) = Hg_resstor(1,:,jres,2) + Hg_resstor(2,:,jres,2)
      hgtot2(:) = Hg_resstor(1,:,jres,3) + Hg_resstor(2,:,jres,3)
      conc_res_hgpsed1(1:2) = hgtot1(1:2) * kd / (10. * ressa*10000. * 5. * (bulk_density * kd * (1. - por_sed) + por_sed)) !mg/kg
      conc_res_hgpsed1(1:2) = hgtot2(1:2) * kd / (10. * ressa*10000. * 15. * (bulk_density * kd * (1. - por_sed) + por_sed)) !mg/kg
      conc_res_hgdsed1(1:2) = conc_res_hgpsed1(1:2) / kd !mg/L
      conc_res_hgdsed2(1:2) = conc_res_hgpsed1(1:2) / kd !mg/L

      kd = 2.9e2 ! (L/KG) -> MeHg
      conc_res_hgpsed1(3) = hgtot1(3) * kd / (10. * ressa*10000. * 5. * (bulk_density * kd + por_sed)) !mg/kg
      conc_res_hgpsed1(3) = hgtot2(3) * kd / (10. * ressa*10000. * 15. * (bulk_density * kd + por_sed)) !mg/kg
      conc_res_hgdsed1(3) = conc_res_hgpsed1(3) / kd !mg/L
      conc_res_hgdsed2(3) = conc_res_hgpsed1(3) / kd !mg/L

      thgdsed = sum(conc_res_hgdsed1(1:3)) !mg/l
      mhgdsed = conc_res_hgdsed1(3)
      
      thgpsed = sum(conc_res_hgpsed1(1:3)) !mg/kg
      mhgpsed = conc_res_hgpsed1(3)
      
      !write(*,'(2i5,4es12.3)') iyr, iida, thgdsed, mhgdsed, thgpsed, mhgpsed
      
      Hg_resstor(1,:,jres,2) = 10. * por_sed * conc_res_hgdsed1(:) * ressa*10000. * 5.  !mg
      Hg_resstor(2,:,jres,2) = hgtot1(:) - Hg_resstor(1,:,jres,2)  !mg
      Hg_resstor(1,:,jres,3) = 10. * por_sed * conc_res_hgdsed2(:) * ressa*10000. * 5. !mg
      Hg_resstor(2,:,jres,3) = hgtot2(:) - Hg_resstor(1,:,jres,3) !mg 

      ! flux of dissolved Hg between water and sediment
      C_hg2d_wtr = Hg_resstor(1,2,jres,1) / (resdepth*100.) ! dissolved Hg2+ concentration in water mg/cm
      C_mehgd_wtr = Hg_resstor(1,3,jres,1) / (resdepth*100.)   ! dissolved MeHg concentration in water mg/cm
      C_hg2d_sed = Hg_resstor(1,2,jres,2) / 5.                  ! dissolved Hg2+ concentration in sediment layer1 mg/cm
      C_mehgd_sed = Hg_resstor(1,3,jres,2) / 5.                 ! dissolved MeHg concentration in sediment layer1 mg/cm
      hg2flux = -0.226 * (C_hg2d_wtr - C_hg2d_sed)*0.1       !dissolved Hg2+ from sediment to water, mg/day
      mehgflux = -0.298 * (C_mehgd_wtr - C_mehgd_sed)*0.1    !dissolved MeHg from sediment to water, mg/day
       
      Hg_resstor(1,2,jres,1) = max(0.,Hg_resstor(1,2,jres,1) + hg2flux) !mg  
      Hg_resstor(1,3,jres,1) = max(0.,Hg_resstor(1,3,jres,1) + mehgflux)
      Hg_resstor(1,2,jres,2) = max(0.,Hg_resstor(1,2,jres,2) - hg2flux)
      Hg_resstor(1,3,jres,2) = max(0.,Hg_resstor(1,3,jres,2) - mehgflux)
       !write(*,*) iyr, iida, sum(dHgp(:)), mehgflux
     
      ! Hg discharge in outflow
      frto = min(1.,resflwo / res_vol(jres))
      varoute(43,ihout) = Hg_resstor(2,1,jres,1) * frto !particulate Hg0 mg
      varoute(44,ihout) = Hg_resstor(2,2,jres,1) * frto !particulate Hg2+ mg
      varoute(45,ihout) = Hg_resstor(2,3,jres,1) * frto !particulate MeHg mg
      varoute(46,ihout) = Hg_resstor(1,1,jres,1) * frto !dissolved Hg0 mg
      varoute(47,ihout) = Hg_resstor(1,2,jres,1) * frto !dissolved Hg2+ mg
      varoute(48,ihout) = Hg_resstor(1,3,jres,1) * frto !dissolved MeHg mg
      
      Hg_resstor(1,1,jres,1) = max(0.,Hg_resstor(1,1,jres,1) - varoute(46,ihout)) !dissolved Hg0 mg
      Hg_resstor(1,2,jres,1) = max(0.,Hg_resstor(1,2,jres,1) - varoute(47,ihout)) !dissolved Hg2+ mg
      Hg_resstor(1,3,jres,1) = max(0.,Hg_resstor(1,3,jres,1) - varoute(48,ihout)) !dissolved MeHg mg
      Hg_resstor(2,1,jres,1) = max(0.,Hg_resstor(2,1,jres,1) - varoute(43,ihout)) !particulate Hg0 mg
      Hg_resstor(2,2,jres,1) = max(0.,Hg_resstor(2,2,jres,1) - varoute(44,ihout)) !particulate Hg2+ mg
      Hg_resstor(2,3,jres,1) = max(0.,Hg_resstor(2,3,jres,1) - varoute(45,ihout)) !particulate MeHg mg
  
      if(Hg_resstor(1,1,jres,1)<0) Hg_resstor(1,1,jres,1) = 0.
      if(Hg_resstor(1,2,jres,1)<0) Hg_resstor(1,1,jres,1) = 0.
      if(Hg_resstor(1,3,jres,1)<0) Hg_resstor(1,1,jres,1) = 0.
      if(Hg_resstor(2,1,jres,1)<0) Hg_resstor(1,1,jres,1) = 0.
      if(Hg_resstor(2,2,jres,1)<0) Hg_resstor(1,1,jres,1) = 0.
      if(Hg_resstor(2,3,jres,1)<0) Hg_resstor(1,1,jres,1) = 0.

      return
      end