      subroutine smrt_read_link

!!    ~ ~ ~ Purpose ~ ~ ~
!!    This subroutine reads in the file containing the information to link SWAT and MODFLOW
!!
!!    ~ ~ ~ Variables Modified ~ ~ ~
!!    name         |units         |definition
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!!    mf_active    |none          |index whether or not to use MODFLOW to
!!                 |              |calculate groundwater flow processes
!!                 |              |instead of SWAT's gwmod
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!!    ~ ~ ~ ~ ~ ~ END SPECIFICATIONS ~ ~ ~ ~ ~ ~

!     Import variables
      use parm, only: nhru,gw_delaye
      use GLOBAL, only: mf_ran,mf_interval !MODFLOW
      use mf_rt_link, only: rt_active !MODFLOW-RT3D Linkage
      use smrt_parm !SWAT-MODFLOW linkage
      implicit none
      
      character*50 header
      integer n,i,j


      !Read in flags for linking SWAT and MODFLOW -----------------------------------------------------------
      open(6001,file='swatmf_link.txt')
      read(6001,*) mf_active
      read(6001,*) mf_irrigation
      read(6001,*) mf_irrigation_swat
      read(6001,*) mf_drain_subs
      read(6001,*) rt_active
      mf_ran = .false.
      mf_interval = 1 !this is set to 1 day (constant for all SWAT-MODFLOW simulations)
      read(6001,*) mf_obs_flag

      !open up log file
      if(mf_active.eq.1) then
        open(6008,file='swatmf_log')
        write(6008,*) 'Progress of SWAT-MODFLOW simulation'
        write(6008,*)
      endif

      !write information to log file
      write(6008,*) 'swatmf_link.txt:    file flags have been read'
      if(mf_active.eq.1) then
        write(6008,*) 'swatmf_link.txt:    MODFLOW is active'
      endif
      if(mf_irrigation.eq.1) then
        write(6008,*) 'swatmf_link.txt:    MODFLOW irrigation is active'
      endif
      if(mf_irrigation_swat.eq.1) then
        write(6008,*) 'swatmf_link.txt:    SWAT irrigation is active'
      endif
      if(mf_drain_subs.eq.1) then
        write(6008,*) 'swatmf_link.txt:    DRAIN cells are active'
      endif
      if(rt_active.eq.1) then
        write(6008,*) 'swatmf_link.txt:    RT3D (N,P) is active'
      endif
      if(mf_obs_flag.eq.1) then
        write(6008,*) 'swatmf_link.txt:    Observation cell file needed'
      endif


      !Optional output files (for SWAT-MODFLOW information) -------------------------------------------------
        
      !SWAT Recharge to Water Table (by SWAT HRU)
      read(6001,*)
      read(6001,*) out_SWAT_recharge 
      if(out_SWAT_recharge.eq.1) then
        open(30001,file='swatmf_out_SWAT_recharge') !rtb
        write(30001,*)'SWAT recharge to water table (mm) (for each HRU)'
      endif

      !MODFLOW Recharge (by MODFLOW cell)
      read(6001,*) out_MF_recharge 
      if(out_MF_recharge.eq.1) then
        open(30002,file='swatmf_out_MF_recharge') !rtb
        write(30002,*) 'MODFLOW Recharge (L3/T) (for each cell)'
        write(30002,*) 'L = length unit used by MODFLOW'
        write(30002,*) 'T = time unit used by MODFLOW'
        write(30002,*) '--Calculated from SWAT HRU recharge--'
      endif

      !SWAT Channel Depth (by SWAT subbasin)
      read(6001,*) out_SWAT_channel 
      if(out_SWAT_channel.eq.1) then
        open(30003,file='swatmf_out_SWAT_channel') !rtb
        write(30003,*) 'SWAT channel depth (m) (for each subbasin)'
      endif

      !MODFLOW River Stage (by MODFLOW River Cell)
      read(6001,*) out_MF_channel
      if(out_MF_channel.eq.1) then
        open(30004,file='swatmf_out_MF_riverstage') !rtb
        write(30004,*) 'MODFLOW River Stage (L) (for each River Cell)'
        write(30004,*) '--Calculated from SWAT Channel Depth--'
      endif

      !GW/SW exchange (by MODFLOW River cell)
      read(6001,*) out_MODFLOW_gwsw 
      if(out_MODFLOW_gwsw.eq.1) then
        open(30005,file='swatmf_out_MF_gwsw') !rtb
        write(30005,*) 'Groundwater/Surface Water exchange (L3/T)'
        write(30005,*) 'L = length unit used by MODFLOW'
        write(30005,*) 'T = time unit used by MODFLOW'
        write(30005,*) 'for each MODFLOW River Cell'
      write(30005,*) 'Positive: River water seeps to the aquifer'
      write(30005,*) 'Negative: Groundwater flows from aquifer to river'
      endif

      !GW/SW exchange (by SWAT subbasin)
      read(6001,*) out_SWAT_gwsw 
      if(out_SWAT_gwsw.eq.1) then
        open(30006,file='swatmf_out_SWAT_gwsw') !rtb
        write(30006,*) 'Groundwater/Surface Water exchange (m3/day)'
        write(30006,*) 'for each SWAT Subbasin'
      write(30006,*) 'Positive: Volume entering stream from the aquifer'
      write(30006,*) 'Negative: Volume seeps from stream to the aquifer'
      endif

      !read flag for printing out monthly and annual average output
      read(6001,*) swatmf_out_avg

      write(6008,*) 'swatmf_link.txt:    output flags have been read'

      !output if RT3D is active
      if(rt_active.eq.1) then
      out_RT_gwsw = 1
      out_RT_rech = 1
      out_SWATNP_gwsw = 1
      out_SWATNP_rech = 1
      
      !GW/SW NO3 exchange (by MODFLOW River Cell) --------------------------------------- NITRATE
      if(out_RT_gwsw.eq.1) then
      open(30007,file='swatmf_out_RT_rivno3') !rtb
      write(30007,*) 'Groundwater/Surface Water NO3 exchange (kg/day)'
      write(30007,*) 'for each MODFLOW River Cell'
      write(30007,*) 'Positive: Mass seeps to the aquifer'
      write(30007,*) 'Negative: Mass from aquifer to river'
      endif
      
      !GW/SW NO3 exchange (by SWAT subbasin)
      if(out_SWATNP_gwsw.eq.1) then
      open(30008,file='swatmf_out_SWAT_rivno3') !rtb
      write(30008,*) 'GW/SW and Drain NO3 exchange (kg/day)'
      write(30008,*) 'for each SWAT Subbasin'
      write(30008,*) 'Positive: Mass entering stream from the aquifer'
      write(30008,*) 'Negative: Mass seeps from stream to the aquifer'
      endif
      
      !NO3 recharge concentration to MODFLOW grid
      if(out_RT_rech.eq.1) then
      open(30009,file='swatmf_out_RT_rechno3') !rtb
      write(30009,*) 'RT3D NO3-N Recharge Conc. (mg/L) for each cell'
      write(30009,*) '--Calculated from SWAT HRU recharge--'
      endif
      
      !NO3 recharge concentration from each SWAT HRU
      if(out_SWATNP_rech.eq.1) then
      open(30010,file='swatmf_out_SWAT_rechno3') !rtb
      write(30010,*) 'SWAT NO3-N Recharge Conc. (mg/L)'
      endif
      
      !GW/SW P exchange (by MODFLOW River Cell) ----------------------------------------- PHOSPHORUS
      if(out_RT_gwsw.eq.1) then
      open(30011,file='swatmf_out_RT_rivP') !rtb
      write(30011,*) 'GW/SW and Drain P exchange (kg/day)'
      write(30011,*) 'for each MODFLOW River Cell'
      write(30011,*) 'Positive: Mass seeps to the aquifer'
      write(30011,*) 'Negative: Mass from aquifer to river'
      endif
      
      !GW/SW P exchange (by SWAT subbasin)
      if(out_SWATNP_gwsw.eq.1) then
      open(30012,file='swatmf_out_SWAT_rivP') !rtb
      write(30012,*) 'Groundwater/Surface Water P exchange (kg/day)'
      write(30012,*) 'for each SWAT Subbasin'
      write(30012,*) 'Positive: Mass entering stream from the aquifer'
      write(30012,*) 'Negative: Mass seeps from stream to the aquifer'
      endif
      
      !P recharge concentration to MODFLOW grid
      if(out_RT_rech.eq.1) then
      open(30013,file='swatmf_out_RT_rechP') !rtb
      write(30013,*) 'RT3D P Recharge Conc. (mg/L) for each cell'
      write(30013,*) '--Calculated from SWAT HRU recharge--'
      endif
      
      !P recharge concentration from each SWAT HRU
      if(out_SWATNP_rech.eq.1) then
      open(30014,file='swatmf_out_SWAT_rechP') !rtb
      write(30014,*) 'SWAT Dissolved P Recharge Conc. (mg/L)'
      endif
      
      !GW/SW Hg exchange (by MODFLOW River Cell) --------------------------------------- MERCURY
      if(out_RT_gwsw.eq.1) then
      open(30015,file='swatmf_out_RT_rivhg') !rtb
      write(30015,*) 'Groundwater/Surface Water Hg exchange (kg/day)'
      write(30015,*) 'for each MODFLOW River Cell'
      write(30015,*) 'Positive: Mass seeps to the aquifer'
      write(30015,*) 'Negative: Mass from aquifer to river'
      endif
      
      !GW/SW Hg exchange (by SWAT subbasin)
      if(out_SWATNP_gwsw.eq.1) then
      open(30016,file='swatmf_out_SWAT_rivhg') !rtb
      write(30016,*) 'GW/SW and Drain Hg exchange (kg/day)'
      write(30016,*) 'for each SWAT Subbasin'
      write(30016,*) 'Positive: Mass entering stream from the aquifer'
      write(30016,*) 'Negative: Mass seeps from stream to the aquifer'
      endif
      
      !Hg recharge concentration to MODFLOW grid
      if(out_RT_rech.eq.1) then
      open(30017,file='swatmf_out_RT_rechhg') !rtb
      write(30017,*) 'RT3D Hg Recharge Conc. (mg/L) for each cell'
      write(30017,*) '--Calculated from SWAT HRU recharge--'
      endif
      
      !Hg recharge concentration from each SWAT HRU
      if(out_SWATNP_rech.eq.1) then
      open(30018,file='swatmf_out_SWAT_rechhg') !rtb
      write(30018,*) 'SWAT Hg Recharge Conc. (mg/L)'
      write(30018,*) 'HRU Year Day Perc(g) Rech(g) Vol(m3) Conc(g/m3)'
      endif
      
      endif


      !Read output control for SWAT-MODFLOW variables -------------------------------------------------------
      read(6001,*)
      read(6001,*) n_outswatmf
      allocate(outswatmf(n_outswatmf))
      do n=1,n_outswatmf
        read(6001,*) outswatmf(n)
      enddo
      swatmf_out_ctr = 1
      write(6008,*) 'swatmf_link.txt:    output control has been read'

      !open files for variable average output (rtb avg) -----------------------------------------------------
      if(swatmf_out_avg) then
        
        !recharge (modflow)
        open(30020,file='swatmf_out_MF_recharge_monthly')
        write(30020,*) 'Monthly Averaged Recharge Values (L3/T) for'
        write(30020,*) 'each MODFLOW grid cell'
        write(30020,*) 'L = length unit used by MODFLOW'
        write(30020,*) 'T = time unit used by MODFLOW'
        write(30020,*) '--Calculated from SWAT HRU recharge--'
        write(30020,*)

        open(30021,file='swatmf_out_MF_recharge_yearly')
        write(30021,*) 'Yearly Averaged Recharge Values (L3/T) for'
        write(30021,*) 'each MODFLOW grid cell'
        write(30021,*) 'L = length unit used by MODFLOW'
        write(30021,*) 'T = time unit used by MODFLOW'
        write(30021,*) '--Calculated from SWAT HRU recharge--'
        write(30021,*)

        !groundwater head
        open(30022,file='swatmf_out_MF_head_monthly')
        write(30022,*) 'Monthly Averaged Head Values from MODFLOW'
        write(30022,*)

        open(30023,file='swatmf_out_MF_head_yearly')
        write(30023,*) 'Yearly Averaged Head Values from MODFLOW'
        write(30023,*)

        !recharge to water table (swat)
        open(30024,file='swatmf_out_SWAT_recharge_monthly')
        write(30024,*) 'Monthly Averaged Recharge Values (mm/day)'
        write(30024,*) 'for each SWAT HRU'
        write(30024,*)

        open(30025,file='swatmf_out_SWAT_recharge_yearly')
        write(30025,*) 'Yearly Averaged Recharge Values (mm/day)'
        write(30025,*) 'for each SWAT HRU'
        write(30025,*)

        !gw/sw exchange (modflow)
        open(30026,file='swatmf_out_MF_gwsw_monthly')
        write(30026,*) 'Monthly Averaged GW/SW Rates (L3/T) for MODFLOW'
        write(30026,*) 'for each MODFLOW River Cell'
        write(30026,*) 'L = length unit used by MODFLOW'
        write(30026,*) 'T = time unit used by MODFLOW'
      write(30026,*) 'Positive: River water seeps to the aquifer'
      write(30026,*) 'Negative: Groundwater flows from aquifer to river'
        write(30026,*)

        open(30027,file='swatmf_out_MF_gwsw_yearly')
        write(30027,*) 'Yearly Averaged GW/SW Rates (L3/T) for MODFLOW'
        write(30027,*) 'L = length unit used by MODFLOW'
        write(30027,*) 'T = time unit used by MODFLOW'
        write(30027,*) 'for each MODFLOW River Cell'
      write(30027,*) 'Positive: River water seeps to the aquifer'
      write(30027,*) 'Negative: Groundwater flows from aquifer to river'
        write(30027,*)

        !gw/sw exchange (swat)
        open(30028,file='swatmf_out_SWAT_gwsw_monthly')
        write(30028,*) 'Monthly Averaged GW/SW Rates (m3/day) for SWAT'
        write(30028,*) 'for each SWAT Subbasin'
      write(30028,*) 'Positive: Volume entering stream from the aquifer'
      write(30028,*) 'Negative: Volume seeps from stream to the aquifer'
        write(30028,*)
      
        open(30029,file='swatmf_out_SWAT_gwsw_yearly')
        write(30029,*) 'Yearly Averaged GW/SW Rates (m3/day) for SWAT'
        write(30029,*) 'for each SWAT Subbasin'
      write(30029,*) 'Positive: Volume entering stream from the aquifer'
      write(30029,*) 'Negative: Volume seeps from stream to the aquifer'
        write(30029,*)

        !solute concentration in groundwater (RT3D)
        if(rt_active.eq.1) then
          open(30030,file='swatmf_out_RT_cno3_monthly')
          write(30030,*) 'Monthly Averaged GW Nitrate'
          write(30030,*) 'Concentration (mg/L) for each RT3D cell'
          write(30030,*)

          open(30031,file='swatmf_out_RT_cno3_yearly')
          write(30031,*) 'Yearly Averaged GW Nitrate'
          write(30031,*) 'Concentration (mg/L) for each RT3D cell'
          write(30031,*)
          
          open(30032,file='swatmf_out_RT_cp_monthly')
          write(30032,*) 'Monthly Averaged GW Phosphorus'
          write(30032,*) 'Concentration (mg/L) for each RT3D cell'
          write(30032,*)

          open(30033,file='swatmf_out_RT_cp_yearly')
          write(30033,*) 'Yearly Averaged GW Phosphorus'
          write(30033,*) 'Concentration (mg/L) for each RT3D cell'
          write(30033,*) 
          
          open(30034,file='swatmf_out_RT_cHg_monthly')
          write(30034,*) 'Monthly Averaged GW Mercury'
          write(30034,*) 'Concentration (mg/L) for each RT3D cell'
          write(30034,*)

          open(30035,file='swatmf_out_RT_cHg_yearly')
          write(30035,*) 'Yearly Averaged GW Mercury'
          write(30035,*) 'Concentration (mg/L) for each RT3D cell'
          write(30035,*) 
          
        endif

      endif

      write(6008,*)

      !check if there are files that will update K and Sy during the simulation
      inquire(file='k_array_seq.txt',exist=ksy_update)
      if(ksy_update) then
        num_updates = 7
        allocate(update_times(num_updates))
        allocate(k_update(252,420,num_updates))
        allocate(sy_update(252,420,num_updates))
        open(225534,file='k_array_seq.txt')
        open(225535,file='sy_array_seq.txt')
        do n=1,num_updates
          read(225534,*) update_times(n)
          do i=1,252
            read(225534,*) (k_update(i,j,n),j=1,420)
          enddo
          read(225535,*)
          do i=1,252
            read(225535,*) (sy_update(i,j,n),j=1,420)
          enddo
        enddo
        ksy_update_num = 1
      endif  
	
      !check if there are HRUs outside the MODFLOW model domain that contribute recharge to MODFLOW cells
      inquire(file='hrus_additional_recharge',exist=outside_recharge)
      if(outside_recharge) then
        open(225536,file='hrus_additional_recharge')
        read(225536,*)
        read(225536,*) num_outside_hrus
        allocate(outside_hrus(num_outside_hrus))
        allocate(outside_rows(num_outside_hrus))
        allocate(outside_cols(num_outside_hrus))
        do n=1,num_outside_hrus
          read(225536,*) outside_hrus(n),
     &                   outside_rows(n),
     &                   outside_cols(n)
        enddo
        open(225537,file='swatmf_out_MF_outside_recharge')
        write(225537,*) 'Recharge from HRUS outside the MODFLOW domain'
      endif
        
      return
      end
