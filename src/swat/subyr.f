      subroutine subyr

!!    ~ ~ ~ PURPOSE ~ ~ ~
!!    this subroutine writes annual subbasin output to the output.sub file

!!    ~ ~ ~ INCOMING VARIABLES ~ ~ ~
!!    name          |units         |definition
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!!    ipdvab(:)     |none          |output variable codes for output.sub file
!!    itotb         |none          |number of output variables printed (output.sub
!!    iyr           |year          |current year of simulation (eg 1980)
!!    msubo         |none          |max number of variables in output.sub file
!!    sub_km(:)     |km^2          |area of subbasin in square kilometers
!!    sub_sw(:)     |mm H2O        |water in soil profile in subbasin
!!    subgis(:)     |none          |GIS code printed to output files(output.sub)
!!    subtot        |none          |number of subbasins in watershed
!!    subyro(1,:)   |mm H2O        |precipitation in subbasin for year
!!    subyro(2,:)   |mm H2O        |snow melt in subbasin for year
!!    subyro(3,:)   |mm H2O        |surface runoff loading in subbasin for year
!!    subyro(4,:)   |mm H2O        |water yield from subbasin for year
!!    subyro(5,:)   |mm H2O        |potential evapotranspiration in subbasin for
!!                                 |year
!!    subyro(6,:)   |mm H2O        |actual evapotranspiration in subbasin for
!!                                 |year
!!    subyro(7,:)   |metric tons/ha|sediment yield from subbasin for year
!!    subyro(8,:)   |kg N/ha       |organic N loading from subbasin for year
!!    subyro(9,:)   |kg P/ha       |organic P loading from subbasin for year
!!    subyro(10,:)  |kg N/ha       |NO3 loading from surface runoff in subbasin
!!                                 |for year
!!    subyro(11,:)  |kg P/ha       |soluble P loading from subbasin for year
!!    subyro(12,:)  |mm H2O        |groundwater loading from subbasin for year
!!    subyro(13,:)  |mm H2O        |percolation out of soil profile in subbasin
!!                                 |for year
!!    subyro(14,:)  |kg P/ha       |loading to reach of mineral P attached to
!!                                 |sediment from subbasin for year
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~

!!    ~ ~ ~ LOCAL DEFINITIONS ~ ~ ~
!!    name        |units         |definition
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!!    ii          |none          |counter
!!    pdvab(:)    |varies        |array to hold subbasin output values
!!    pdvb(:)     |varies        |array of user selected subbasin output
!!                               |values
!!    sb          |none          |subbasin number
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~

!!    ~ ~ ~ ~ ~ ~ END SPECIFICATIONS ~ ~ ~ ~ ~ ~

      use parm

      integer :: sb, ii
      real, dimension (msubo) :: pdvab, pdvb

      do sb = 1, subtot

        pdvab = 0.
        pdvb = 0.
  
        pdvab(1) = subyro(1,sb)
        pdvab(2) = subyro(2,sb)
        pdvab(3) = subyro(5,sb)
        pdvab(4) = subyro(6,sb)
        pdvab(5) = sub_sw(sb)
        pdvab(6) = subyro(13,sb)
        pdvab(7) = subyro(3,sb)
        pdvab(8) = subyro(12,sb)
        pdvab(9) = subyro(4,sb)
        pdvab(10) = subyro(7,sb)
        pdvab(11) = subyro(8,sb)
        pdvab(12) = subyro(9,sb)
        pdvab(13) = subyro(10,sb)
        pdvab(14) = subyro(11,sb)
        pdvab(15) = subyro(14,sb)
        pdvab(16) = subyro(15,sb)
        pdvab(17) = subyro(16,sb)
        pdvab(18) = subyro(17,sb)
!!      chl_a, cbodu and doxq were all written out in daily
!!      output code.  not set up for monthly/yearly
!!      all values will be zero for all codes except daily
!!      added for jennifer b.
        pdvab(19) = 0.0
        pdvab(20) = 0.0
        pdvab(21) = 0.0
        pdvab(22) = subyro(18,sb)    !!tile_no3
        !Mercury output Jaehak 2017
        pdvab(25) = subyro(21,sb)
        pdvab(26) = subyro(22,sb)
        pdvab(27) = subyro(23,sb)
        pdvab(28) = subyro(24,sb)
        pdvab(29) = subyro(25,sb)
        pdvab(30) = subyro(26,sb)
        pdvab(31) = subyro(27,sb)
        pdvab(32) = subyro(28,sb)
        pdvab(33) = subyro(29,sb)
        pdvab(34) = subyro(30,sb)
        pdvab(35) = subyro(31,sb)  / Real(366 - leapyr)
        pdvab(36) = subyro(32,sb)  / Real(366 - leapyr)
        pdvab(37) = subyro(33,sb)  / Real(366 - leapyr)
        pdvab(38) = subyro(34,sb)  / Real(366 - leapyr)
        pdvab(39) = subyro(35,sb)  / Real(366 - leapyr)
        pdvab(40) = subyro(36,sb)  / Real(366 - leapyr)
        pdvab(41) = subyro(37,sb)  / Real(366 - leapyr)
        pdvab(42) = subyro(38,sb)  / Real(366 - leapyr)
        pdvab(43) = subyro(39,sb)  / Real(366 - leapyr)
        pdvab(44) = subyro(40,sb)  / Real(366 - leapyr)
        pdvab(45) = subyro(41,sb)  / Real(366 - leapyr)
        pdvab(46) = subyro(42,sb)  / Real(366 - leapyr)
        pdvab(47) = subyro(43,sb)  / Real(366 - leapyr)
        pdvab(48) = subyro(44,sb)  / Real(366 - leapyr)
        pdvab(49) = subyro(45,sb)  / Real(366 - leapyr)
        pdvab(50) = subyro(46,sb)
        pdvab(51) = subyro(47,sb)
        pdvab(52) = subyro(48,sb)
        pdvab(53) = subyro(49,sb)
        pdvab(54) = subyro(50,sb)
        pdvab(55) = subyro(51,sb)
        pdvab(56) = subyro(52,sb)
        pdvab(57) = subyro(53,sb)
        pdvab(58) = subyro(54,sb)
        pdvab(59) = subyro(55,sb)
        pdvab(60) = subyro(56,sb)
        pdvab(61) = subyro(57,sb)
        pdvab(62) = subyro(58,sb)
        pdvab(63) = subyro(59,sb)
        pdvab(64) = subyro(60,sb)
        pdvab(65) = subyro(61,sb) 
        pdvab(66) = subyro(62,sb) 
        pdvab(67) = subyro(63,sb) 
        pdvab(68) = subyro(64,sb) / Real(366 - leapyr)
        pdvab(69) = subyro(65,sb) 

        if (ipdvab(1) > 0) then
          do ii = 1, itotb
            pdvb(ii) = pdvab(ipdvab(ii))
          end do
          write (31,1000) sb,  iyr, 0,0,sub_km(sb),              
     &                                    (pdvb(ii), ii = 1, 24)
          write (7746118,1000) sb,  iyr, 0,0,sub_km(sb),              
     &                                    (pdvb(ii), ii = 25, itotb)
        else
          write (31,1000) sb,  iyr, 0,0,sub_km(sb),              
     &                                    (pdvab(ii), ii = 1, 24)
          write (7746118,1000) sb,  iyr, 0,0,sub_km(sb),              
     &                                    (pdvab(ii), ii = 25, msubo)
        end if
      end do

      return
!1000 format ('BIGSUB',i4,1x,i8,1x,i4,e10.5,21f10.3)
 1000 format ('BIGSUB',i4,1x,i4,1x,i4,1x,i4,1x,e10.3,64e12.3)
      end 