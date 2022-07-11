      subroutine subaa(years)

!!    ~ ~ ~ PURPOSE ~ ~ ~
!!    this subroutine writes average annual subbasin output to the output.sub file

!!    ~ ~ ~ INCOMING VARIABLES ~ ~ ~
!!    name          |units         |definition
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!!    ipdvab(:)     |none          |output variable codes for output.sub file
!!    itotb         |none          |number of output variables printed (output.sub)
!!    msubo         |none          |max number of variables in output.sub file
!!    sub_km(:)     |km^2          |area of subbasin in square kilometers
!!    sub_sw(:)     |mm H2O        |water in soil profile in subbasin
!!    subaao(1,:)   |mm H2O        |precipitation in subbasin for simulation
!!    subaao(2,:)   |mm H2O        |snow melt in subbasin for simulation
!!    subaao(3,:)   |mm H2O        |surface runoff loading in subbasin for 
!!                                 |simulation
!!    subaao(4,:)   |mm H2O        |water yield from subbasin for simulation
!!    subaao(5,:)   |mm H2O        |potential evapotranspiration in subbasin for
!!                                 |simulation
!!    subaao(6,:)   |mm H2O        |actual evapotranspiration in subbasin for
!!                                 |simulation
!!    subaao(7,:)   |metric tons/ha|sediment yield from subbasin for simulation
!!    subaao(8,:)   |kg N/ha       |organic N loading from subbasin for 
!!                                 |simulation
!!    subaao(9,:)   |kg P/ha       |organic P loading from subbasin for
!!                                 |simulation
!!    subaao(10,:)  |kg N/ha       |NO3 loading from surface runoff in subbasin
!!                                 |for simulation
!!    subaao(11,:)  |kg P/ha       |soluble P loading from subbasin for 
!!                                 |simulation
!!    subaao(12,:)  |mm H2O        |groundwater loading from subbasin for 
!!                                 |simulation
!!    subaao(13,:)  |mm H2O        |percolation out of soil profile in subbasin
!!                                 |for simulation
!!    subaao(14,:)  |kg P/ha       |loading to reach of mineral P attached to
!!                                 |sediment from subbasin for simulation
!!    subaao(18,i)  |              |groundwater***?
!!    subgis(:)     |none          |GIS code printed to output files(output.sub)
!!    subtot        |none          |number of subbasins in watershed
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~

!!    ~ ~ ~ LOCAL DEFINITIONS ~ ~ ~
!!    name        |units         |definition
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!!    ii          |none          |counter
!!    pdvab(:)    |varies        |array to hold subbasin output values
!!    pdvb(:)     |varies        |array of user selected subbasin output
!!                               |values
!!    sb          |none          |subbasin number
!!    years       |years         |length of simulation
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~

!!    ~ ~ ~ ~ ~ ~ END SPECIFICATIONS ~ ~ ~ ~ ~ ~

      use parm

      real, intent (in) :: years
      integer :: sb, ii
      real, dimension (msubo) :: pdvab, pdvb

      do sb = 1, subtot

        pdvab = 0.
        pdvb = 0.
  
        pdvab(1) = subaao(1,sb)
        pdvab(2) = subaao(2,sb)
        pdvab(3) = subaao(5,sb)
        pdvab(4) = subaao(6,sb)
        pdvab(5) = sub_sw(sb)
        pdvab(6) = subaao(13,sb)
        pdvab(7) = subaao(3,sb)
        pdvab(8) = subaao(12,sb)
        pdvab(9) = subaao(4,sb)
        pdvab(10) = subaao(7,sb)
        pdvab(11) = subaao(8,sb)
        pdvab(12) = subaao(9,sb)
        pdvab(13) = subaao(10,sb)
        pdvab(14) = subaao(11,sb)
        pdvab(15) = subaao(14,sb)
        pdvab(16) = subaao(15,sb)
        pdvab(17) = subaao(16,sb)
        pdvab(18) = subaao(17,sb)
!!      chl_a, cbodu and doxq were all written out in daily
!!      output code.  not set up for monthly/yearly
!!      all values will be zero for all codes except daily
!!      added for jennifer b.
        pdvab(19) = 0.0
        pdvab(20) = 0.0
        pdvab(21) = 0.0
        pdvab(22) = subaao(18,sb)   !!tile_no3
 !Mercury output Jaehak 2017
        pdvab(25) = subaao(21,sb)
        pdvab(26) = subaao(22,sb)
        pdvab(27) = subaao(23,sb)
        pdvab(28) = subaao(24,sb)
        pdvab(29) = subaao(25,sb)
        pdvab(30) = subaao(26,sb)
        pdvab(31) = subaao(27,sb)
        pdvab(32) = subaao(28,sb)
        pdvab(33) = subaao(29,sb)
        pdvab(34) = subaao(30,sb)
        pdvab(35) = subaao(31,sb)  / Real(366 - leapyr)
        pdvab(36) = subaao(32,sb)  / Real(366 - leapyr)
        pdvab(37) = subaao(33,sb)  / Real(366 - leapyr)
        pdvab(38) = subaao(34,sb)  / Real(366 - leapyr)
        pdvab(39) = subaao(35,sb)  / Real(366 - leapyr)
        pdvab(40) = subaao(36,sb)  / Real(366 - leapyr)
        pdvab(41) = subaao(37,sb)  / Real(366 - leapyr)
        pdvab(42) = subaao(38,sb)  / Real(366 - leapyr)
        pdvab(43) = subaao(39,sb)  / Real(366 - leapyr)
        pdvab(44) = subaao(40,sb)  / Real(366 - leapyr)
        pdvab(45) = subaao(41,sb)  / Real(366 - leapyr)
        pdvab(46) = subaao(42,sb)  / Real(366 - leapyr)
        pdvab(47) = subaao(43,sb)  / Real(366 - leapyr)
        pdvab(48) = subaao(44,sb)  / Real(366 - leapyr)
        pdvab(49) = subaao(45,sb)  / Real(366 - leapyr)
        pdvab(50) = subaao(46,sb)
        pdvab(51) = subaao(47,sb)
        pdvab(52) = subaao(48,sb)
        pdvab(53) = subaao(49,sb)
        pdvab(54) = subaao(50,sb)
        pdvab(55) = subaao(51,sb)
        pdvab(56) = subaao(52,sb)
        pdvab(57) = subaao(53,sb)
        pdvab(58) = subaao(54,sb)
        pdvab(59) = subaao(55,sb)
        pdvab(60) = subaao(56,sb)
        pdvab(61) = subaao(57,sb)
        pdvab(62) = subaao(58,sb)
        pdvab(63) = subaao(59,sb)
        pdvab(64) = subaao(60,sb)
        pdvab(65) = subaao(61,sb) 
        pdvab(66) = subaao(62,sb)
        pdvab(67) = subaao(63,sb)
        pdvab(68) = subaao(64,sb) / Real(366 - leapyr)
        pdvab(69) = subaao(65,sb)

        if (ipdvab(1) > 0) then
          do ii = 1, itotb
            pdvb(ii) = pdvab(ipdvab(ii))
          end do
          write (31,1000) sb, years,0,0, sub_km(sb),            
     &                                    (pdvb(ii), ii = 1, 24)
          write (7746118,1000) sb, years,0,0, sub_km(sb),            
     &                                    (pdvb(ii), ii = 25, itotb)
        else
          write (31,1000) sb, subgis(sb), years, sub_km(sb),            
     &                                    (pdvab(ii), ii = 1, 24)
          write (7746118,1000) sb, subgis(sb), years, sub_km(sb),            
     &                                    (pdvab(ii), ii = 25, msubo)
        end if
      end do

      return
!1000 format ('BIGSUB',i4,1x,i8,1x,f4.1,e10.5,21f10.3)
 1000 format ('BIGSUB',i4,1x,f4.1,1x,i4,1x,i4,1x,e10.3,64e12.3)
      end 