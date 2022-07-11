      subroutine subday

!!    ~ ~ ~ PURPOSE ~ ~ ~
!!    this subroutine writes daily subbasin output to the output.sub file

!!    ~ ~ ~ INCOMING VARIABLES ~ ~ ~
!!    name          |units         |definition
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!!    da_ha         |ha            |area of watershed in hectares
!!    iida          |julian date   |current day of simulation
!!    ipdvab(:)     |none          |output variable codes for output.sub file
!!    itotb         |none          |number of output variables printed (output.sub)
!!    msubo         |none          |max number of variables in output.sub file
!!    sub_etday(:)  |mm H2O        |actual evapotranspiration on day in subbasin
!!    sub_fr(:)     |none          |fraction of watershed area in subbasin
!!    sub_gwq(:)    |mm H2O        |groundwater flow on day in subbasin
!!    sub_km(:)     |km^2          |area of subbasin in square kilometers
!!    sub_no3(:)    |kg N/ha       |NO3-N in surface runoff on day in subbasin
!!    sub_pet(:)    |mm H2O        |potential evapotranspiration for day in
!!                                 |subbasin
!!    sub_qd(:)     |mm H2O        |surface runoff loading to main channel on
!!                                 |day in subbasin
!!    sub_sedpa(:)  |kg P/ha       |amount of active mineral P attached to 
!!                                 |sediment removed in surface runoff on day 
!!                                 |in subbasin
!!    sub_sedps(:)  |kg P/ha       |amount of stable mineral P attached to 
!!                                 |sediment removed in surface runoff on day 
!!                                 |in subbasin
!!    sub_sedy(:)   |metric tons   |sediment yield for the day in subbasin
!!    sub_sep(:)    |mm H2O        |seepage from bottom of soil profile on day
!!                                 |in subbasin
!!    sub_snom(:)   |mm H2O        |snow melt on day in subbasin
!!    sub_solp(:)   |kg P/ha       |soluble P in surface runoff on day in 
!!                                 |subbasin
!!    sub_subp(:)   |mm H2O        |precipitation for day in subbasin
!!    sub_sw(:)     |mm H2O        |water in soil profile in subbasin
!!    sub_wyld(:)   |mm H2O        |water yield on day in subbasin
!!    sub_yorgn(:)  |kg N/ha       |organic N in surface runoff on day in 
!!                                 |subbasin
!!    sub_yorgp(:)  |kg P/ha       |organic P in surface runoff on day in 
!!                                 |subbasin
!!    subgis(:)     |none          |GIS code printed to output files(output.sub)
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~

!!    ~ ~ ~ LOCAL DEFINITIONS ~ ~ ~
!!    name        |units         |definition
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!!    ii          |none          |counter
!!    pdvab(:)    |varies        |array to hold subbasin output values
!!    pdvb(:)     |varies        |array of user selected subbasin output
!!                               |values
!!    sb          |none          |subbasin number
!!    sub_ha      |ha            |area of subbasin in hectares
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~

!!    ~ ~ ~ ~ ~ ~ END SPECIFICATIONS ~ ~ ~ ~ ~ ~

      use parm
      use smrt_parm, only: mf_active,sub_gw_output

      integer :: sb, ii
      real :: sub_ha
      real, dimension (msubo) :: pdvab, pdvb

      sb = 0
      sb = hru_sub(ihru)

      sub_ha = 0.
      sub_ha = da_ha * sub_fr(sb)

      pdvab = 0.
      pdvb = 0.

      pdvab(1) = sub_subp(sb)
      pdvab(2) = sub_snom(sb)
      pdvab(3) = sub_pet(sb)
      pdvab(4) = sub_etday(sb)
      pdvab(5) = sub_sw(sb)
      pdvab(6) = sub_sep(sb)
      pdvab(7) = sub_qd(sb)
      if(mf_active.eq.1) then
        pdvab(8) = sub_gw_output(sb) !rtb modflow
      else
        pdvab(8) = sub_gwq(sb) 
      endif
      pdvab(9) = sub_wyld(sb)
      pdvab(10) = sub_sedy(sb)/ sub_ha
      pdvab(11) = sub_yorgn(sb)
      pdvab(12) = sub_yorgp(sb)
      pdvab(13) = sub_no3(sb)
      pdvab(14) = sub_solp(sb)
      pdvab(15) = sub_sedpa(sb) + sub_sedps(sb)
      pdvab(16) = sub_latq(sb)
      pdvab(17) = sub_latno3(sb)
      pdvab(18) = sub_gwno3(sb)
      pdvab(19) = sub_chl(sb) / sub_ha
      pdvab(20) = sub_cbod(sb) / sub_ha
      pdvab(21) = sub_dox(sb) / sub_ha
      pdvab(22) = sub_tileno3(sb)
              !Mercury output Jaehak 2017

      pdvab(25) = Hg_dry_total_sub(1,2,sb)     !total dissolved Hg2 dry deposition (mg/ha)
      pdvab(26) = Hg_dry_total_sub(2,2,sb)     !total particulate Hg2+ dry deposition  (mg/ha)
      pdvab(27) = Hg_dry_leaf_sub(1,2,sb)     !leaf dissolved Hg2 dry deposition  (mg/ha)
      pdvab(28) = Hg_dry_leaf_sub(2,2,sb)     !DIRECT particulate Hg2+ dry deposition (mg/ha)
      pdvab(29) = Hg_wet_total_sub(1,2,sb)     !! total dissolved Hg2+ wet deposition (mg/ha)
      pdvab(30) = Hg_wet_direct_sub(1,2,sb)     !! direct dissolved Hg2+ wet deposition, (mg/ha)
      pdvab(31) = Hg_wet_thru_sub(1,2,sb)      !throughfall dissolved Hg2+ wet deposition, (mg/ha)
      pdvab(32) = Hg_leaf_fall_sub(1,1,sb)     !leaf fall of dissolved Hg0  (mg/ha)
      pdvab(33) = Hg_leaf_fall_sub(1,2,sb)     !leaf fall of dissolved Hg2+  (mg/ha)
      pdvab(34) = Hg_leaf_fall_sub(2,2,sb)     !leaf fall of particulate Hg2+  (mg/ha)
      pdvab(35) = Hg_leaf_sub(1,1,sb)     !leaf storage of dissolved Hg0  (mg/ha)
      pdvab(36) = Hg_leaf_sub(1,2,sb)     !leaf storage of dissolved Hg+ (mg/ha)
      pdvab(37) = Hg_leaf_sub(2,2,sb)     !leaf storage of particulate Hg2+ (mg/ha)
      pdvab(38) = Hg_residue_sub(1,1,sb)     !residue storage of dissolved Hg0 (mg/ha)
      pdvab(39) = Hg_residue_sub(1,2,sb)     !residue storage of dissolved Hg2+ (mg/ha)
      pdvab(40) = Hg_residue_sub(2,2,sb)     !residue storage of particulate Hg2+ (mg/ha)
      pdvab(41) = Hg_soil_sub(1,1,sb)     !soil storage of dissolved Hg0 (mg/ha)
      pdvab(42) = Hg_soil_sub(1,2,sb)     !soil storage of dissolved Hg2+(mg/ha)
      pdvab(43) = Hg_soil_sub(1,3,sb)     !soil storage of dissolved MeHg (mg/ha)
      pdvab(44) = Hg_soil_sub(2,1,sb)     !soil storage of particulate Hg0  (mg/ha)
      pdvab(45) = Hg_soil_sub(2,2,sb)     !soil storage of particulate Hg2+ (mg/ha)
      pdvab(46) = Hg_soil_sub(2,3,sb)     !soil storage of particulate MeHg (mg/ha)
      pdvab(47) = Hg_shallst_sub(1,1,sb)     !shallow aquifer storage of dissolved Hg0 (mg/ha)
      pdvab(48) = Hg_shallst_sub(1,2,sb)     !shallow aquifer storage of dissolved Hg2+ (mg/ha)
      pdvab(49) = Hg_shallst_sub(1,3,sb)     !shallow aquifer storage of dissolved MeHg (mg/ha)
      pdvab(50) = Hg_surq_sub(1,sb)     !dissolved Hg0 in runoff (mg/ha)
      pdvab(51) = Hg_surq_sub(2,sb)     !dissolved Hg2+ in runoff (mg/ha)
      pdvab(52) = Hg_surq_sub(3,sb)     !dissolved MeHg in runoff (mg/ha)
      pdvab(53) = Hg_latq_sub(1,sb)     !dissolved Hg0 in lateral flow (mg/ha)
      pdvab(54) = Hg_latq_sub(2,sb)     !dissolved Hg2+ in lateral flow (mg/ha)
      pdvab(55) = Hg_latq_sub(3,sb)     !dissolved MeHg in lateral flow (mg/ha)
      pdvab(56) = Hg_perc_sub(1,sb)     !dissolved Hg0 percolated to shallow aquifer  (mg/ha)
      pdvab(57) = Hg_perc_sub(2,sb)     !dissolved Hg2+ percolated to shallow aquifer (mg/ha)
      pdvab(58) = Hg_perc_sub(3,sb)     !dissolved MeHg percolated to shallow aquifer  (mg/ha)
      pdvab(59) = Hg_gwq_sub(1,sb)     !dissolved Hg0 in groundwater return flow  (mg/ha)
      pdvab(60) = Hg_gwq_sub(2,sb)     !dissolved Hg2+ in groundwater return flow (mg/ha)
      pdvab(61) = Hg_gwq_sub(3,sb)     !dissolved MeHg in groundwater return flow (mg/ha)
      pdvab(62) = Hg_sedyld_sub(1,sb)     !particulate Hg0 in sediment yield (mg/ha)
      pdvab(63) = Hg_sedyld_sub(2,sb)     !particulate Hg2+ in sediment yield (mg/ha)
      pdvab(64) = Hg_sedyld_sub(3,sb)     !particulate MeHg in sediment yield (mg/ha)
      pdvab(65) =  Hg_wyld_sub(1,sb)    !total Hg0 in water yield(mg/ha)
      pdvab(66) =  Hg_wyld_sub(2,sb)    !total Hg2+ in water yield(mg/ha)
      pdvab(67) =  Hg_wyld_sub(3,sb)    !total MeHg in water yield(mg/ha)
      pdvab(68) =  Hg_sc_sub(sb)    !total Hg concentration in soil (ng/kg)
      pdvab(69) =  Hg0_vola_sub(sb)    !Hg0 volatilization from soil to atmosphere(mg/ha)
      
      if (ipdvab(1) > 0) then
        do ii = 1, itotb
          pdvb(ii) = pdvab(ipdvab(ii))
        end do
        if (icalen== 0) then
            write (31,1000)sb, iyr,mo_chk, iida, sub_km(sb),
     &                                    (pdvb(ii), ii = 1, 24)
            write (7746118,1000)sb, iyr,mo_chk, iida, sub_km(sb),
     &                                    (pdvb(ii), ii = 25, itotb)
        end if
        
        if (icalen== 1) then
            write (31,1001)sb, iyr, i_mo, icl(iida), 
     &          sub_km(sb), (pdvb(ii), ii = 1, 24)
            write (7746118,1001)sb, iyr, i_mo, icl(iida), 
     &          sub_km(sb), (pdvb(ii), ii = 25, itotb)
        end if
         
!!    added for binary files 3/25/09 gsm line below and write (66666
	      if (ia_b == 1) then
	        write (66666) sb, subgis(sb), iida, sub_km(sb),               
     &                                        (pdvb(ii), ii = 1, itotb)
	      endif
      else
        if (icalen== 0) then
            write (31,1000) sb, subgis(sb), iida, sub_km(sb),
     &                                (pdvab(ii), ii = 1, 24)
            write (7746118,1000) sb, subgis(sb), iida, sub_km(sb),
     &                                (pdvab(ii), ii = 25, msubo)
        endif
        
        if (icalen== 1) then
            write (31,1001) sb, subgis(sb), i_mo, icl(iida), 
     &         iyr, sub_km(sb), (pdvab(ii), ii = 1, 24)
            write (7746118,1001) sb, subgis(sb), i_mo, icl(iida), 
     &         iyr, sub_km(sb), (pdvab(ii), ii = 25, msubo)
        endif
        
!!    added for binary files 3/25/09 gsm line below and write (6666
	        if (ia_b == 1) then
                write(66666) sb, subgis(sb), iida, sub_km(sb),        
     &                                        (pdvab(ii), ii = 1, msubo)
              endif
        
	end if


      return
!     changed for jennifer b.
!1000 format ('BIGSUB',i4,1x,i8,1x,i4,e10.5,18f10.3)
!1000 format ('BIGSUB',i4,1x,i8,1x,i4,e10.5,21f10.3)
 1000 format ('BIGSUB',i4,1x,i4,1x,i4,1x,i4,1x,e10.3,64e12.3)
 1001 format ('BIGSUB',i4,1x,i4,1x,i4,1x,i4,1x,e10.3,64e12.3)
						 
      end 