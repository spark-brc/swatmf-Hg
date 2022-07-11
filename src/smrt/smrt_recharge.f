      subroutine smrt_recharge

!!    ~ ~ ~ PURPOSE ~ ~ ~
!!    This subroutine converts the necessary SWAT variables for the MODFLOW recharge
!!    (RCH) package's variables
!!      
        !Import variables
        use parm, only:day_total,rchrg
        use GLOBAL, only: NCOL,NROW,ibound,DELR,DELC
        use GWFRCHMODULE, only: RECH
        use smrt_parm
        implicit none
        
        !Define local variables
        integer i,j,k
        integer cell_row,cell_col,hru_id
        real    cell_area,recharge


        !convert to MODFLOW Recharge array
        call smrt_dhru2grid2D(rchrg_dhru,RECH,1)

        !add HRU recharge from outside the MODFLOW model domain
        if(outside_recharge) then
          write(225537,*)
          write(225537,*) 'Recharge (m3/day) for day:',day_total
          write(225537,*) 'HRU, Row, Column, Recharge'
          do i=1,num_outside_hrus
            cell_row = outside_rows(i)
            cell_col = outside_cols(i)
            hru_id = outside_hrus(i)
            cell_area = DELR(cell_col) * DELC(cell_row) !m2
            recharge = (rchrg(hru_id)/1000.) * cell_area !mm --> m3/day
            RECH(cell_col,cell_row) = RECH(cell_col,cell_row) + recharge
            write(225537,101) hru_id,cell_row,cell_col,recharge
          enddo
        endif
        
        !calculate average recharge for the current month (rtb avg)
        smrt_RECH_tot_mo = smrt_RECH_tot_mo + RECH
        smrt_RECH_avg_mo = smrt_RECH_tot_mo / day_count_mo

        !calculate average recharge for the current year (rtb avg)
        smrt_RECH_tot_yr = smrt_RECH_tot_yr + RECH
        smrt_RECH_avg_yr = smrt_RECH_tot_yr / day_count_yr

        !if the cell is inactive, set value to 0 (to prevent large negative values)
        do i=1,nrow
          do j=1,ncol
            if(ibound(j,i,1).eq.0) then
              RECH(j,i) = 0.0
            endif
          enddo
        enddo
        
        !output Recharge values to a file
        if(out_MF_recharge.eq.1 .and.
     &    day_total.eq.outswatmf(swatmf_out_ctr)) then
          write(30002,*)
          write(30002,*) 'Day:',day_total
          write(30002,*) 'daily recharge values provided to MODFLOW'  
          do i=1,nrow
            write(30002,100) (RECH(j,i),j=1,ncol)
          enddo
        endif

        
  100 format(1000(e17.5))
  101 format(i8,i8,i8,f18.4)

      end subroutine smrt_recharge 
