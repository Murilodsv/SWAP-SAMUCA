    subroutine Samuca(task)

    use Variables
    use var_samuca

    implicit none


    integer     icrop_ini
    integer     icrop_end
    integer     i

    integer		task                                  ! Controler of call ordering from the main program
    integer		method_ws                             ! Water Stress Method:  1 = linear response; 2 = asymptote response
    integer		method_pop                            ! Tillering Method:     1 = Cdays; 2 = Cdays + Light Transmission; 3 = Source-Sink
    integer		nseason                               !
    integer		atln                                  !
    integer		atln_now                              !
    integer		dn_lf_alive_dewlap                    !
    integer		ghour                                 !
    integer		glai                                  !
    integer		maxdgl                                !
    integer		maxgl                                 !
    integer		n_it                                  !
    integer		n_it_ag                               !
    integer		n_it_bg                               !
    integer		n_lf                                  !
    integer		n_lf_ag                               !
    integer		n_lf_ag_dewlap                        !
    integer		n_lf_alive                            !
    integer		n_lf_alive_ag                         !
    integer		n_lf_alive_bg                         !
    integer		n_lf_alive_dewlap                     !
    integer		n_lf_alive_juveni                     !
    integer		n_lf_alive_juveni_ag                  !
    integer		n_lf_alive_juveni_bg                  !
    integer		n_lf_bg                               !
    integer		n_lf_dead                             !
    integer		n_lf_dead_ag                          !
    integer		n_lf_dead_bg                          !
    integer		n_lf_it_form                          !
    integer		n_lf_when_stk_emerg                   !
    integer		n_ph                                  !
    integer		n_ph_ag                               !
    integer		n_ph_bg                               !
    integer		nphy_bground                          !
    integer		nsublay_cm                            !
    integer		phy                                   !
    integer		pos_it_bg                             !
    integer		sl                                    !
    integer     sub_sl                                !
    integer     n_sub_sl                              !
    integer		tl                                    !
    logical		fl_potential                          !
    logical		fl_appear_leaf                        !
    logical		fl_hasleaf                            !
    logical		fl_it_visible                         !
    logical		fl_lf_ag_phy                          !
    logical		fl_shed_leaf                          !
    logical		fl_stalk_emerged                      !
    logical		fl_tiller_decrease                    !
    logical		fl_tiller_increase                    !
    logical		fl_tiller_peaked                      !
    logical		fl_tiller_stop                        !
    logical		fl_use_reserves                       !
    real        agefactor_fac_amax                    !
    real        agefactor_fac_rue                     !
    real        agefactor_fac_per                     !
    real        a_pl                                  !
    real        b_pl                                  !
    real        c_pl                                  !
    real        max_lf_dw                             !
    real        init_stalkfw                          !
    real        init_stalkht                          !
    real        nstalks_planting                      !
    real        ini_nstk                              !
    real        tilleragefac                          !
    real		initcropdepth                         !
    real		init_plantdepth_ratoon                !
    real		dw_rt                                 !
    real		max_rt_dw                             !
    real		dw_it_bg                              !
    real		str_it_bg                             !
    real		sug_it_bg                             !
    real		suc_it_bg                             !
    real		hex_it_bg                             !
    real		dw_it                                 !
    real		ini_dw_rt                             !
    real		rootleftfrac                          !
    real		dw_total                              !
    real		age_it_phy                            !
    real		age_lf_phy                            !
    real		agefactor_amax                        !
    real		agefactor_per                         !
    real		agefactor_rue                         !
    real		amax_conv                             !
    real		amax_mod                              !
    real		amax_out                              !
    real		amaxfbfac                             !
    real		avail_subs_crop                       !
    real		c_check_tol                           !
    real		c_scattering                          !
    real		can_ipar                              !
    real		chudec_lt                             !
    real		chumat_lt                             !
    real		co2_pho_res_end                       !
    real		co2_pho_res_ini                       !
    real		cr_source_sink_ratio                  !
    real		cr_source_sink_ratio_ruse             !
    real		dage_it_phy                           !
    real		dage_lf_phy                           !
    real		ddw_it                                !
    real		ddw_it_ag                             !
    real		ddw_it_ag_dead                        !
    real		ddw_it_bg                             !
    real		ddw_it_bg_dead                        !
    real		ddw_it_dead                           !
    real		ddw_it_phy_growth                     !
    real		ddw_it_phy_reserves                   !
    real		ddw_lf                                !
    real		ddw_lf_ag                             !
    real		ddw_lf_appear                         !
    real		ddw_lf_bg                             !
    real		ddw_lf_dead                           !
    real		ddw_lf_shed                           !
    real		ddw_rt                                !
    real		ddw_rt_dead                           !
    real		dead_lai                              !
    real		diac_at_emergence                     !
    real		diacem                                !
    real		diacsoil                              !
    real		diacsoilem                            !
    real		diair                                 !
    real		diam_stk                              !
    real		diphy                                 !
    real		disoil                                !
    real		dla_gain_ref_till                     !
    real		dla_phy                               !
    real		dlai_dead                             !
    real		dlai_gain                             !
    real		dlai_gain_appear                      !
    real		dlai_shed                             !
    real		dnstk                                 !
    real		dnstk_dead_rate                       !
    real		dphy_stimuli                          !
    real		dr_itss                               !
    real		dr_lfss                               !
    real		dr_rtss                               !
    real		drdepth                               !
    real		dshootext_bg                          !
    real		dshootext_bg_rate                     !
    real		dstr_it_ag                            !
    real		dstr_it_ag_dead                       !
    real		dstr_it_bg                            !
    real		dstr_it_bg_dead                       !
    real		dstr_it_phy                           !
    real		dstr_it_phy_growth                    !
    real		dsubsres                              !
    real		dsubsres_it                           !
    real		dsubsres_lf                           !
    real		dsubsres_ratio                        !
    real		dsubsres_rt                           !
    real		dsug_corr_fac_ag                      !
    real		dsug_corr_fac_bg                      !
    real		dsug_it_ag                            !
    real		dsug_it_ag_dead                       !
    real		dsug_it_bg                            !
    real		dsug_it_bg_dead                       !
    real		dsug_it_phy                           !
    real		dsug_it_phy_growth                    !
    real		dsug_it_phy_reserves                  !
    real		dswat_ddws                            !
    real		dswat_dsuc                            !
    real		dtcrss                                !
    real		dtg                                   !
    real		dtg_avail_it                          !
    real		dtg_avail_it_ag                       !
    real		dtg_avail_it_ag_ref_till              !
    real		dtg_avail_it_bg                       !
    real		dtg_avail_it_bg_ref_till              !
    real		dtg_avail_it_phy                      !
    real		dtg_avail_it_ref_till                 !
    real		dtg_avail_lf                          !
    real		dtg_avail_lf_phy                      !
    real		dtg_avail_lf_ref_till                 !
    real		dtg_avail_rt                          !
    real		dtitss                                !
    real		dtitss_ag                             !
    real		dtitss_ag_ref_till                    !
    real		dtitss_bg                             !
    real		dtitss_bg_ref_till                    !
    real		dtitss_phy                            !
    real		dtitss_ref_till                       !
    real		dtlfss                                !
    real		dtlfss_phy                            !
    real		dtlfss_ref_till                       !
    real		dtot_str_dw_ref_till                  !
    real		dtrtss                                !
    real		dw_aerial                             !
    real		dw_it_phy                             !
    real		dw_lf                                 !
    real		dw_lf_ag                              !
    real		dw_lf_bg                              !
    real		dw_lf_phy                             !
    real		dw_lf_shed_phy                        !
    real		dw_ss_it                              !
    real		dw_ss_it_phy                          !
    real		dw_ss_lf                              !
    real		dw_ss_lf_phy                          !
    real		dw_ss_rt                              !
    real		dwat_it_ag                            !
    real		dwat_it_ag_dead                       !
    real		eff_conv                              !
    real		eff_mod                               !
    real		eff_out                               !
    real		effective_rd                          !
    real		end_tt_it_growth                      !
    real		end_tt_lf_growth                      !
    real		end_tt_rt_growth                      !
    real		exc_dtg_it                            !
    real		exc_dtg_lf                            !
    real		exc_dtg_rt                            !
    real		fdeadlf                               !
    real		frac_ag                               !
    real		frac_bg                               !
    real		frac_hex_bg                           !
    real		frac_li                               !
    real		frac_suc_bg                           !
    real		fw_it_ag                              !
    real		gresp                                 !
    real		gresp_it                              !
    real		gresp_it_phy                          !
    real		gresp_lf                              !
    real		gresp_lf_phy                          !
    real		gresp_rt                              !
    real		hex_it_ag                             !
    real		hex_it_ag_ref_till                    !
    real		hex_it_bg_ref_till                    !
    real		hex_it_phy                            !
    real		hex_min                               !
    real		hour                                  !
    real		ini_dw_lf_phy                         !
    real		ini_la                                !
    real		init_leaf_area                        !
    real		it_struc_pfac                         !
    real		it_struc_pfac_delta                   !
    real		it_struc_pfac_max                     !
    real		it_struc_pfac_min                     !
    real		it_struc_pfac_rate                    !
    real		it_struc_pfac_tb                      !
    real		it_struc_pfac_te                      !
    real		it_struc_pfac_temp_max_red            !
    real		it_struc_pfac_tm                      !
    real		it_struc_pfac_wate_max_red            !
    real		it_struc_tb_end                       !
    real		it_struc_tb_ini                       !
    real		it_struc_to1                          !
    real		it_struc_to2                          !
    real		k_can                                 !
    real		kmr_it_phy                            !
    real		kmr_leaf                              !
    real		kmr_root                              !
    real		kmr_stem                              !
    real		kmr_stor                              !
    real		la_lf_shed_phy                        !
    real		lai_ass                               !
    real		laimod                                !
    real		lf_dpos                               !
    real		lgpf                                  !
    real		lt                                    !
    real		ltthreshold                           !
    real		maintenance_factor_crop               !
    real		maintenance_factor_it                 !
    real		maintenance_factor_it_ag              !
    real		maintenance_factor_it_bg              !
    real		maintenance_factor_it_phy             !
    real		maintenance_factor_lf                 !
    real		maintenance_factor_lf_phy             !
    real		maintenance_factor_rt                 !
    real		max_ini_la                            !
    real		max_it_dw                             !
    real		max_it_dw_bg                          !
    real		max_it_dw_phy                         !
    real		max_per_it                            !
    real		mid_tt_it_growth                      !
    real		mid_tt_lf_growth                      !
    real		mid_tt_rt_growth                      !
    real		mresp_it                              !
    real		mresp_it_phy                          !
    real		mresp_lf                              !
    real		mresp_lf_phy                          !
    real		mresp_rt                              !
    real		n_lf_max_ini_la                       !
    real		n_lf_tiller                           !
    real		nsenesleaf_effect                     !
    real		nstk_at_appearance                    !
    real		nstk_now                              !
    real		par_rad                               !
    real		per                                   !
    real		per_hour                              !
    real		per_it_phy                            !
    real		pho_fac_co2                           !
    real		phy_stimuli                           !
    real		phyllochron                           !
    real		plastochron                           !
    real		poppeak_lt                            !
    real		q10_it_phy                            !
    real		q10_leaf                              !
!    real		q10_root                              !
    real		q10_stem                              !
    real		q10_stor                              !
    real		rdprof                                !
    real		reduc_growth_factor_crop              !
    real		reduc_growth_factor_it                !
    real		reduc_growth_factor_it_ag             !
    real		reduc_growth_factor_it_bg             !
    real		reduc_growth_factor_it_phy            !
    real		reduc_growth_factor_lf                !
    real		reduc_growth_factor_lf_phy            !
    real		reduc_growth_factor_rt                !
    real		rel_ss_it_phy                         !
    real		rel_ss_lf_phy                         !
    real		res_used_emerg                        !
    real		res_used_emerg_fac                    !
    real		reserves_used_growth_it               !
    real		reserves_used_growth_lf               !
    real		reserves_used_growth_rt               !
    real		reserves_used_mresp_crop              !
    real		reserves_used_mresp_it                !
    real		reserves_used_mresp_it_ag             !
    real		reserves_used_mresp_it_bg             !
    real		reserves_used_mresp_it_phy            !
    real		reserves_used_mresp_lf                !
    real		reserves_used_mresp_lf_phy            !
    real		reserves_used_mresp_rt                !
    real		rgpf                                  !
    real		root_front_size                       !
    real		rootdrate                             !
    real		rootshape                             !
    real		rpup                                  !
    real		rue_mod                               !
    real		shared_it_str_bg                      !
    real		shared_it_sug_bg                      !
    real		shootdepth                            !
    real		soiltemperature                       !
    real		srlmax                                !
    real		srlmin                                !
    real		stk_h                                 !
    real		str_it_ag                             !
    real		str_it_phy                            !
    real		subs_avail_growth_crop                !
    real		subs_avail_growth_it                  !
    real		subs_avail_growth_it_ag               !
    real		subs_avail_growth_it_ag_ref_till      !
    real		subs_avail_growth_it_bg               !
    real		subs_avail_growth_it_bg_ref_till      !
    real		subs_avail_growth_it_phy              !
    real		subs_avail_growth_it_ref_till         !
    real		subs_avail_growth_lf                  !
    real		subs_avail_growth_lf_phy              !
    real		subs_avail_growth_lf_ref_till         !
    real		subs_avail_growth_rt                  !
    real		subsres                               !
    real		subsres_avail_it                      !
    real		subsres_avail_it_ag                   !
    real		subsres_avail_it_ag_ref_till          !
    real		subsres_avail_it_bg                   !
    real		subsres_avail_it_bg_ref_till          !
    real		subsres_avail_it_phy                  !
    real		subsres_avail_it_ref_till             !
    real		subsres_avail_lf                      !
    real		subsres_avail_lf_phy                  !
    real		subsres_avail_lf_ref_till             !
    real		subsres_avail_rt                      !
    real		suc_acc_ini                           !
    real		suc_frac_rate_ts                      !
    real		suc_it_ag                             !
    real		suc_it_ag_ref_till                    !
    real		suc_it_bg_ref_till                    !
    real		suc_it_phy                            !
    real		suc_min                               !
    real		sug_cont                              !
    real		sug_it_ag                             !
    real		sug_it_phy                            !
    real		sup_ratio_it                          !
    real		sup_ratio_it_ag                       !
    real		sup_ratio_it_bg                       !
    real		sup_ratio_it_phy                      !
    real		sup_ratio_lf                          !
    real		sup_ratio_lf_phy                      !
    real		sup_ratio_rt                          !
    real		supply_rate_it                        !
    real		supply_rate_it_ag                     !
    real		supply_rate_it_bg                     !
    real		supply_rate_it_phy                    !
    real		supply_rate_lf                        !
    real		supply_rate_lf_phy                    !
    real		supply_rate_rt                        !
    real		supply_used_crop                      !
    real		supply_used_dw_crop                   !
    real		supply_used_dw_it                     !
    real		supply_used_dw_it_ag                  !
    real		supply_used_dw_it_bg                  !
    real		supply_used_dw_it_phy                 !
    real		supply_used_dw_lf                     !
    real		supply_used_dw_lf_phy                 !
    real		supply_used_dw_rt                     !
    real		supply_used_gresp_crop                !
    real		supply_used_gresp_it                  !
    real		supply_used_gresp_it_ag               !
    real		supply_used_gresp_it_bg               !
    real		supply_used_gresp_it_phy              !
    real		supply_used_gresp_lf                  !
    real		supply_used_gresp_lf_phy              !
    real		supply_used_gresp_rt                  !
    real		supply_used_it                        !
    real		supply_used_it_ag                     !
    real		supply_used_it_bg                     !
    real		supply_used_it_phy                    !
    real		supply_used_lf                        !
    real		supply_used_lf_phy                    !
    real		supply_used_mresp_crop                !
    real		supply_used_mresp_it                  !
    real		supply_used_mresp_it_ag               !
    real		supply_used_mresp_it_bg               !
    real		supply_used_mresp_it_phy              !
    real		supply_used_mresp_lf                  !
    real		supply_used_mresp_lf_phy              !
    real		supply_used_mresp_rt                  !
    real		supply_used_rt                        !
    real		t_mresp                               !
    real		tb0pho                                !
    real		tb1pho                                !
    real		tb2pho                                !
    real		tbfpho                                !
    real		tbmax_per                             !
    real		temperature_factor                    !
    real		tempfac_per                           !
    real		tempfac_pho                           !
    real		tilleragefac_adjust                   !
    real		tillochron                            !
    real		tot_dw_ss_crop                        !
    real		tot_dw_ss_it                          !
    real		tot_dw_ss_it_ag                       !
    real		tot_dw_ss_it_bg                       !
    real		tot_dw_ss_lf                          !
    real		tot_dw_ss_rt                          !
    real		tot_gresp_crop                        !
    real		tot_gresp_it                          !
    real		tot_gresp_it_ag                       !
    real		tot_gresp_it_bg                       !
    real		tot_gresp_lf                          !
    real		tot_gresp_rt                          !
    real		tot_mresp_crop                        !
    real		tot_mresp_it                          !
    real		tot_mresp_it_ag                       !
    real		tot_mresp_it_bg                       !
    real		tot_mresp_lf                          !
    real		tot_mresp_rt                          !
    real		tref_mr                               !
    real		tref_mr_it_phy                        !
    real		ts_it_phy                             !
    real		tt_chumat_lt                          !
    real		wat_con                               !
    real		wat_it_ag                             !

    !--- Crop parameters
    real        amax
    real        phtmax
    real        parmax
    real        ccmp
    real        ccmax
    real        cceff
    real        rue
    real        tb
    real        tbper
    real        chustk
    real        chupeak
    real        chudec
    real        chumat
    real        popmat
    real		POPPEAK
    real*8		SLA_SAM ! MODIFIED TO SLA_SAM TO AVOID CONFLICT WITH 'use variables' statement
    real		RDM
    real		DPERCOEFF
    real		MLA
    real		KC_MIN
    real		EORATIO
    real		RWUEP1
    real		RWUEP2
    real		T_MAX_WS_PHO
    real		T_MID_WS_PHO
    real		T_MIN_WS_PHO
    real		T_MAX_WS_EXP
    real		T_MID_WS_EXP
    real		T_MIN_WS_EXP
    real		MAXLAI_EO
    real		TBM
    real		THRESHEWS
    real		SWCON1
    real		SWCON2
    real		SWCON3
    real		RWUMAX
    real		PORMIN
    real		T_MAX_WS_FPF
    real		T_MID_WS_FPF
    real		T_MIN_WS_FPF
    real		T_MAX_WS_TIL
    real		T_MID_WS_TIL
    real	    T_MIN_WS_TIL
    !logical     POTENTIAL_GROWTH
    !integer     TILLERMET
    !real 	    ROWSP
    integer     SEQNOW
    logical     RATOON

    real        BOTTOM(maho)
    real        DEP(maho)
    logical     FLCROPALIVE
    !real        PLANTDEPTH

    real        SLTHICKNESS(maho)
    real        tsoil_lay(maho)
!    real        SRL
    real        UPPER(maho)
    real        RLD(maho)
    !logical 	USETSOIL
    real 		TRWUP
    real 		TMIN
    real 		TMAX
    real 		THOUR(24)
    real 		SWFACT
    real 		SWFACP
    real 		SWFACF
    real 		SWFACE
    !real 		SRAD*8
    integer 	NDWS
    integer 	NDEWS
    !logical 	MULCHEFFECT
    !integer 	METPG
    real 		LI
    real 		LAT_SIM
    real 		EOP
    real 		DTGA
    integer 	DOY
    real 		DILEAF
    real 		DI
    !real 		CO2
    integer  	YEAR
    logical  	WRITEACTOUT
    integer  	WARN
    real  		TRASW
    real  		RESP
    real  		POL
    integer  	OUTDPP
    real  		KC
    integer  	DAS
    integer  	DAP
    !real*8      dso
    !real*8      dsinbe
    !real*8      dsinb
    !real*8      cosld
    !real*8      dayl
    !real*8      sinld
    real        tmed

    integer     outp
    integer     outd
    integer     outds
    integer     outdph
    integer     outdpa
    integer     outpfac
    integer     outstres
    integer     outcumd
    logical     writedetphoto
    logical     writedcrop
    logical     writehead
    logical     writecumdens
    real        dw_it_ag
    real        nstk
    real        diac
    real        sgpf
    logical     flemerged
    logical     flinit_file
    logical     flclos_file
    logical     flprompt_msg
    logical     detailedsoil
    real*8      depth                                           ! Dynamic variable for cumdens
    real        rootdis(202)                                    ! Dynamic array for cumdens
    real        soma                                            ! Dynamic variable for cumdens
    integer     cumdenstb                                       ! Flag to use RDCTB to scale cumdens (cumdenstb=1) or SAMUCA's root length density (cumdenstb=0)


    character 	(len = 6)	pltype          				    !  Planting type (Ratoon or PlCane)
    character 	(len = 6)	cropstatus          			    !  Dead or Alive
    character 	(len = 6)	cropdstage          			    !  Development Stage - Only Sprout or Emergd

    !--- Arrays Variables
    real		phprof(100,60)                                  ! Phytomer profile and attributes dimensions
    real		drld_sl(maho)                                 !
    real		dw_rt_sl(maho)                                !
    real		ddw_rt_sl(maho)                               !
    real		srl_prof(1000)                                  !
    real		ddw_rt_prof(1000)                               !
    real		drld_prof(1000)                                 !
    real		geot(maho)                                    !
    real		rootprof(1000)                                  ! Root profile (index = cm comparment)    Up to 10 meters
    real		dw_rt_prof(maho)                              !
    real		tillerageprof(100,2)                            !
    real		tempfac_h_per(24)                               ! 24 hours
    real		Acanopy(3+1,5+1)                                ! Instantaneous CO2 Assimilation Rate at three hours and five canopy depth in kg(CO2) ha-1(leaf) h-1
    real		Qleaf(3+1,5+1)                                  ! Instantaneous par absorbed by leaves at three hours and five canopy depth in W m-2
    real		incpar(3,4)                                     ! Incoming direct, difuse and total par radiation above canopy in three hours W m-2
    real		photo_layer_act(3)                              ! Actual Total Daily Photosynthesis per canopy Layer
    real		rgf(maho+1,3)                                 !
    real		lroot(maho)                                   !
    real		dlroot(maho)                                  !
    real		drld(maho)                                    !
    real		drld_dead(maho)                               !
    real*8      relative_rld(maho)
    logical		fl_it_AG(100)                                   ! Above Ground Internode Flag
    logical		fl_lf_AG(100)                                   ! Above Ground Leaf Flag
    logical		fl_lf_alive(100)

    real        array_deb(MAHO)
    real*8      dsinbe_sam

    !--- Real Functions
    real*8		afgen                                           ! Interpolation function (The Fortran Simulation Translator, FST version 2.0)
    real		fgrowth                                         ! Flexible growth function
    real		asy_ws                                          ! Flexible function for water stress response
    real		tiller_senes                                    ! Tiller senescence function
    
    real top_dummy, bot_dummy

    save

    goto (1000,2000,3000) task

    !--- dev notes:
    !   nsublay changed to      nsublay_cm
    !   k       changed to      k_can
    !   nlay    changed to      numlay
    !   check sequential issues: file initialization, rotooning flags, etc...               []
    !   make sure all control parameters are readable or set in the code                    [OK]
    !   retrieve slthickness, bottom, upper and dep from swap                               [OK]
    !   retrieve environmental variables from swap (temperature, rain, radiation, etc...)   []
    !   check if tsoil is calculated for subcompartments or by soil layers                  [OK]
    !   review water_stress to cope with swap

    ! Arrays from control
    ! ratoon
    ! rowsp
    ! plantdepth
    ! potential_growth
    ! tillermet
    ! usetsoil
    
!   !Soil Surface Residue
 !   call readint('Soil Surface Residue',1,ir_id,size(ir_id),.false.,.false.,ctrl,ctrlfile(sn),msg)
 !   call readint('Soil Surface Residue',2,yearmulch,size(yearmulch),.false.,.false.,ctrl,ctrlfile(sn),msg)
 !   call readint('Soil Surface Residue',3,doymulch,size(doymulch),.false.,.false.,ctrl,ctrlfile(sn),msg)
 !   call readint('Soil Surface Residue',4,restype,size(restype),.false.,.false.,ctrl,ctrlfile(sn),msg)
 !   call readrea('Soil Surface Residue',5,mumass,size(mumass),.false.,.false.,ctrl,ctrlfile(sn),msg)
 !   call readrea('Soil Surface Residue',6,muwatfac,size(muwatfac),.false.,.false.,ctrl,ctrlfile(sn),msg)
 !   call readrea('Soil Surface Residue',7,muam,size(muam),.false.,.false.,ctrl,ctrlfile(sn),msg)
 !   call readrea('Soil Surface Residue',8,muext,size(muext),.false.,.false.,ctrl,ctrlfile(sn),msg)
 !   call readrea('Soil Surface Residue',9,mualb,size(mualb),.false.,.false.,ctrl,ctrlfile(sn),msg)
 !   call readrea('Soil Surface Residue',10,lam_mu_dry,size(lam_mu_dry),.false.,.false.,ctrl,ctrlfile(sn),msg)
 !   call readrea('Soil Surface Residue',11,lam_dmu_wet,size(lam_dmu_wet),.false.,.false.,ctrl,ctrlfile(sn),msg)
 !   call readrea('Soil Surface Residue',12,max_mulch_evap,size(max_mulch_evap),.false.,.false.,ctrl,ctrlfile(sn),msg)
 !   
 !   !Simulation Options
 !   call readint('Simulation Options',1,ir_id,size(ir_id),.false.,.false.,ctrl,ctrlfile(sn),msg)
 !   call readint('Simulation Options',2,mesev,size(mesev),.false.,.false.,ctrl,ctrlfile(sn),msg)
 !   call readint('Simulation Options',3,petmethod,size(petmethod),.false.,.false.,ctrl,ctrlfile(sn),msg)
 !   call readint('Simulation Options',4,stempm,size(stempm),.false.,.false.,ctrl,ctrlfile(sn),msg)
 !   call readint('Simulation Options',5,SwBotbHea,size(SwBotbHea),.false.,.false.,ctrl,ctrlfile(sn),msg)
 !   call readint('Simulation Options',6,P_type,size(P_type),.false.,.false.,ctrl,ctrlfile(sn),msg)
 !   call readint('Simulation Options',7,metpg,size(metpg),.false.,.false.,ctrl,ctrlfile(sn),msg)
 !   call readint('Simulation Options',8,metpart,size(metpart),.false.,.false.,ctrl,ctrlfile(sn),msg)
 !   call readint('Simulation Options',9,tillermet,size(tillermet),.false.,.false.,ctrl,ctrlfile(sn),msg)
 !   call readlog('Simulation Options',10,usetsoil,size(usetsoil),.false.,.false.,ctrl,ctrlfile(sn),msg)
 !   call readlog('Simulation Options',11,sw_mulcheffect,size(sw_mulcheffect),.false.,.false.,ctrl,ctrlfile(sn),msg)
 !   call readlog('Simulation Options',12,potential_growth,size(potential_growth),.false.,.false.,ctrl,ctrlfile(sn),msg)
 !   call readlog('Simulation Options',13,flstandalone,size(flstandalone),.false.,.false.,ctrl,ctrlfile(sn),msg)
 !   
 !   !Methods Parameters
 !   call readint('Methods Parameters',1,ir_id,size(ir_id),.false.,.false.,ctrl,ctrlfile(sn),msg)
 !   call readrea('Methods Parameters',2,slu1_rd,size(slu1_rd),.false.,.false.,ctrl,ctrlfile(sn),msg)
 !   call readrea('Methods Parameters',3,hrnc_rd,size(hrnc_rd),.false.,.false.,ctrl,ctrlfile(sn),msg)
 !   call readrea('Methods Parameters',4,dhrl_rd,size(dhrl_rd),.false.,.false.,ctrl,ctrlfile(sn),msg)
 !   call readrea('Methods Parameters',5,drya1_rd,size(drya1_rd),.false.,.false.,ctrl,ctrlfile(sn),msg)
 !   call readrea('Methods Parameters',6,drya2_rd,size(drya2_rd),.false.,.false.,ctrl,ctrlfile(sn),msg)
 !   call readrea('Methods Parameters',7,dryb1_rd,size(dryb1_rd),.false.,.false.,ctrl,ctrlfile(sn),msg)
 !   call readrea('Methods Parameters',8,dryb2_rd,size(dryb2_rd),.false.,.false.,ctrl,ctrlfile(sn),msg)
 !   call readrea('Methods Parameters',9,sweta_rd,size(sweta_rd),.false.,.false.,ctrl,ctrlfile(sn),msg)
 !   call readrea('Methods Parameters',10,swetb_rd,size(swetb_rd),.false.,.false.,ctrl,ctrlfile(sn),msg)
 !   call readrea('Methods Parameters',11,swequa_rd,size(swequa_rd),.false.,.false.,ctrl,ctrlfile(sn),msg)
 !   call readrea('Methods Parameters',12,swequb_rd,size(swequb_rd),.false.,.false.,ctrl,ctrlfile(sn),msg)    
 !   call readrea('Methods Parameters',13,tbot_mean,size(tbot_mean),.false.,.false.,ctrl,ctrlfile(sn),msg)
 !   call readrea('Methods Parameters',14,tbot_ampli,size(tbot_ampli),.false.,.false.,ctrl,ctrlfile(sn),msg)
 !   call readrea('Methods Parameters',15,tbot_imref,size(tbot_imref),.false.,.false.,ctrl,ctrlfile(sn),msg)
 !   call readrea('Methods Parameters',16,tbot_ddamp,size(tbot_ddamp),.false.,.false.,ctrl,ctrlfile(sn),msg)
 !   

1000 continue

    !--------------------------!
    !--- Simulation Options ---!
    !--------------------------!

    !--- Get parameters from Samuca.mng called in readswap.for L.287
    writeactout         = .true.
    writedetphoto       = .false.
    writedcrop          = .false.
    writehead           = .true.
    writecumdens        = .true.
    detailedsoil        = .true.
    flprompt_msg        = .false.
    
    !-----------------!    
    !--- HARDWIRED ---!
    !-----------------!    
    !--- Hardwired to provide parameters that are still not readable from SWAP-control files:
    co2             =   400.        ! HARDWIRED FOR SWAP
    rowsp           =   140.        ! HARDWIRED FOR SWAP    
    potential_growth= .false.       ! HARDWIRED FOR SWAP
    swdrought       = 1             ! HARDWIRED FOR SWAP [feddes]
    swoxygen        = 1
    tillermet       = 2
    metpg           = 2
    swinter         = 1
    swcf            = 1             ! use Kc from SAMUCA
    idev            = 1
    
    !--- Use prescribed root density profile 
    !--- cumdenstb = 1: reads root factors from 'SamucaRDCTB.par', converts it into cumdens and use to distribute RLD in soil layers [SRL=slrmin]
    !--- cumdenstb = 0: simulates RLD using native SAMUCA approach [e.g. Vianna etal 2020] and update cumdens based on simulated RLD
    cumdenstb       = 1
    
    !--- Use grass values if none is prescribed in par file
    HLIM1  =      0.0
    HLIM2U =      1.0
    HLIM2L =     -1.0    
    HLIM3H =    -200.0
    HLIM3L =    -800.0
    HLIM4  =   -8000.0
    ADCRH  =       0.5
    ADCRL  =       0.1
    ALPHACRIT =    0.7
    
    !-------------------------------!
    !--- Reading crop parameters ---!
    !-------------------------------!

    !--- size of parameters arrays
    n_inte_host = 7
    n_real_host = 118

    !--- read from 'Samuca.par'
    call ReadFile_samuca(6, n_inte_host, inte_host, n_real_host,real_host)

    !--- Passing read parameters values
    nsenesleaf_effect             = inte_host(  1) ! (I)
    maxgl                         = inte_host(  2) ! (I)
    n_lf_max_ini_la               = inte_host(  3) ! (I)
    n_lf_when_stk_emerg           = inte_host(  4) ! (I)
    n_lf_it_form                  = inte_host(  5) ! (I)
    maxdgl                        = inte_host(  6) ! (I)
    schedule                      = inte_host(  7) ! (I) ! IRRIGATION SCHEDULER OF SWAP: =1 [ON] =0 [OFF] [IRRIGATION SETUP WILL BE READ FROM Sugarcane.crp]
    amax                          = real_host(  1) ! (R)
    eff                           = real_host(  2) ! (R)
    phtmax                        = real_host(  3) ! (R)
    parmax                        = real_host(  4) ! (R)
    ccmp                          = real_host(  5) ! (R)
    ccmax                         = real_host(  6) ! (R)
    cceff                         = real_host(  7) ! (R)
    rue                           = real_host(  8) ! (R)
    tb                            = real_host(  9) ! (R)
    tb0pho                        = real_host( 10) ! (R)
    tb1pho                        = real_host( 11) ! (R)
    tb2pho                        = real_host( 12) ! (R)
    tbfpho                        = real_host( 13) ! (R)
    tbper                         = real_host( 14) ! (R)
    tbMax_per                     = real_host( 15) ! (R)
    chustk                        = real_host( 16) ! (R)
    chupeak                       = real_host( 17) ! (R)
    chudec                        = real_host( 18) ! (R)
    chumat                        = real_host( 19) ! (R)
    popmat                        = real_host( 20) ! (R)
    poppeak                       = real_host( 21) ! (R)
    ltthreshold                   = real_host( 22) ! (R)
    tillochron                    = real_host( 23) ! (R)
    fdeadlf                       = real_host( 24) ! (R)
    phyllochron                   = real_host( 25) ! (R)
    sla_sam                       = real_host( 26) ! (R)
    rdm                           = real_host( 27) ! (R)
    srlMax                        = real_host( 28) ! (R)
    srlMin                        = real_host( 29) ! (R)
    rootdrate                     = real_host( 30) ! (R)
    max_rt_dw                     = real_host( 31) ! (R)
    end_tt_rt_growth              = real_host( 32) ! (R)
    rootleftfrac                  = real_host( 33) ! (R)
    kdif                          = real_host( 34) ! (R)
    dpercoeff                     = real_host( 35) ! (R)
    mla                           = real_host( 36) ! (R)
    kc_min                        = real_host( 37) ! (R)
    eoratio                       = real_host( 38) ! (R)
    rwuep1                        = real_host( 39) ! (R)
    rwuep2                        = real_host( 40) ! (R)
    t_max_ws_pho                  = real_host( 41) ! (R)
    t_mid_ws_pho                  = real_host( 42) ! (R)
    t_min_ws_pho                  = real_host( 43) ! (R)
    t_max_ws_exp                  = real_host( 44) ! (R)
    t_mid_ws_exp                  = real_host( 45) ! (R)
    t_min_ws_exp                  = real_host( 46) ! (R)
    plastochron                   = real_host( 47) ! (R)
    frac_suc_BG                   = real_host( 48) ! (R)
    frac_hex_BG                   = real_host( 49) ! (R)
    init_leaf_area                = real_host( 50) ! (R)
    max_ini_la                    = real_host( 51) ! (R)
    cr_source_sink_ratio_ruse     = real_host( 52) ! (R)
    init_plantdepth_ratoon        = real_host( 53) ! (R)
    maxlai_eo                     = real_host( 54) ! (R)
    gresp                         = real_host( 55) ! (R)
    kmr_leaf                      = real_host( 56) ! (R)
    kmr_stem                      = real_host( 57) ! (R)
    kmr_root                      = real_host( 58) ! (R)
    kmr_stor                      = real_host( 59) ! (R)
    q10_leaf                      = real_host( 60) ! (R)
    q10_stem                      = real_host( 61) ! (R)
    q10_root                      = real_host( 62) ! (R)
    q10_stor                      = real_host( 63) ! (R)
    tref_mr                       = real_host( 64) ! (R)
    tbm                           = real_host( 65) ! (R)
    threshews                     = real_host( 66) ! (R)
    dshootext_BG_rate             = real_host( 67) ! (R)
    mid_tt_rt_growth              = real_host( 68) ! (R)
    it_struc_tb_ini               = real_host( 69) ! (R)
    it_struc_to1                  = real_host( 70) ! (R)
    it_struc_to2                  = real_host( 71) ! (R)
    it_struc_tb_end               = real_host( 72) ! (R)
    max_it_dw                     = real_host( 73) ! (R)
    mid_tt_it_growth              = real_host( 74) ! (R)
    end_tt_it_growth              = real_host( 75) ! (R)
    mid_tt_lf_growth              = real_host( 76) ! (R)
    end_tt_lf_growth              = real_host( 77) ! (R)
    it_struc_pfac_max             = real_host( 78) ! (R)
    it_struc_pfac_min             = real_host( 79) ! (R)
    it_struc_pfac_tb              = real_host( 80) ! (R)
    it_struc_pfac_tm              = real_host( 81) ! (R)
    it_struc_pfac_te              = real_host( 82) ! (R)
    it_struc_pfac_delta           = real_host( 83) ! (R)
    it_struc_pfac_temp_max_red    = real_host( 84) ! (R)
    it_struc_pfac_wate_max_red    = real_host( 85) ! (R)
    max_it_dw_BG                  = real_host( 86) ! (R)
    suc_min                       = real_host( 87) ! (R)
    max_per_it                    = real_host( 88) ! (R)
    tilleragefac_adjust           = real_host( 89) ! (R)
    dswat_ddws                    = real_host( 90) ! (R)
    dswat_dsuc                    = real_host( 91) ! (R)
    rootshape                     = real_host( 92) ! (R)
    hex_min                       = real_host( 93) ! (R)
    suc_acc_ini                   = real_host( 94) ! (R)
    suc_frac_rate_ts              = real_host( 95) ! (R)
    swcon1                        = real_host( 96) ! (R)
    swcon2                        = real_host( 97) ! (R)
    swcon3                        = real_host( 98) ! (R)
    rwumax                        = real_host( 99) ! (R)
    pormin                        = real_host(100) ! (R)
    tt_chumat_lt                  = real_host(101) ! (R)
    res_used_emerg_fac            = real_host(102) ! (R)
    agefactor_fac_amax            = real_host(103) ! (R)
    agefactor_fac_rue             = real_host(104) ! (R)
    agefactor_fac_per             = real_host(105) ! (R)
    c_scattering                  = real_host(106) ! (R)
    k_can                         = real_host(107) ! (R)
    root_front_size               = real_host(108) ! (R)
    HLIM1                         = real_host(109) ! (R)     0.0
    HLIM2U                        = real_host(110) ! (R)     1.0
    HLIM2L                        = real_host(111) ! (R)    -1.0    
    HLIM3H                        = real_host(112) ! (R)   -200.0
    HLIM3L                        = real_host(113) ! (R)   -800.0
    HLIM4                         = real_host(114) ! (R)  -8000.0
    ADCRH                         = real_host(115) ! (R)      0.5
    ADCRL                         = real_host(116) ! (R)      0.1
    ALPHACRIT                     = real_host(117) ! (R)    0.7
    plantdepth                    = real_host(118) ! (R) IN SAMUCA, this is a management parameter, but we decided to keep it here to not change SWAP.swp or to create new input file only for 1 variable...
    
    !--- read root profile if necessary
    !--- initialise and start reading
    if (cumdenstb .eq. 1) then
        call ReadSamucaRDCTB(pathcrop, cropfil(icrop),cumdens)    
    endif    
      
    !--- root density is 
    ! top = abs((z(node) + 0.5*dz(node)) / rd)
    ! bot = abs((z(node) - 0.5*dz(node)) / rd)
    ! frac_root_i = (afgen(cumdens,202,bot)-afgen(cumdens,202,top))
    
    !--- Convert parameters for i/o purpose
    agefactor_fac_amax           = agefactor_fac_amax / 1.e5
    agefactor_fac_rue            = agefactor_fac_rue  / 1.e5
    agefactor_fac_per            = agefactor_fac_per  / 1.e5

    !--- Assume same response for tillering and partitioning factor
    t_max_ws_fpf    = t_max_ws_exp
    t_mid_ws_fpf    = t_mid_ws_exp
    t_min_ws_fpf    = t_min_ws_exp
    t_max_ws_til    = t_max_ws_pho
    t_mid_ws_til    = t_mid_ws_pho
    t_min_ws_til    = t_min_ws_pho

    !--- Species-related response
    co2_pho_res_end =   270.d0
    co2_pho_res_ini =   0.d0

    !--- retrieve soil vertical discretization from 'Swap.swp'
    slthickness = hsublay(1:numlay)
    do sl = 1, numlay
        if (sl == 1)then
            dep(sl) = slthickness(sl)
        else
            dep(sl) = dep(sl-1) + slthickness(sl)
        endif
        upper(sl)  = dep(sl) - slthickness(sl)
        bottom(sl) = dep(sl)       
        
    enddo

    !--- Get initial and final icrop rotation IDs for sugarcane
    i           = 1
    icrop_end   = 0
    do while(cropstart(i) .gt. 0.0001)
        if(croptype(i) .eq. 4) icrop_end = i
        i = i + 1
    enddo
    i           = 1
    do while(cropstart(i) .gt. 0.0001 .and. &
     & .not. (croptype(i) .eq. 4))
        i = i + 1
    enddo
    icrop_ini = i

    !--- Update flags and seq ID
    if(icrop .eq. icrop_ini)then
        seqnow      =   1
        flinit_file =   .true.
        flclos_file =   .false.
    else
        seqnow      =   seqnow  +   1
        flinit_file =   .false.
        if(icrop .eq. icrop_end) flclos_file =   .true.
    endif

    !--- Check if is a ratoon type
    if(cropname(icrop) .eq. 'Sugarcane_R') then
        ratoon = .true.
    else
        ratoon = .false.
    endif
    
    !--- Evapotranspiration setup
    if (swcf.eq.1) then
        !--- use standard values for ETref (FAO56) and employ Kc for ETc
        albedo = 0.23d0
        rsc = 70.0d0
        rsw = 0.0d0
    else
        write(*,*) &
        & 'SWCF = 0. Please provide sugarcane albedo, rsc and rsw in'// &
        & ' file.crp'
    endif

    ch    =   0.d0      ! Crop Height [cm] for PenMon()
    kc    =   kc_min    ! Crop factor (-)
    cf    =   kc        ! Crop factor for SWAP (-)
    kdir  =   kdif

    !--- Simulate Water Stress
    fl_potential      = potential_growth

    !---------------!
    !--- Methods ---!
    !---------------!

    !--- Water Stress Response
    method_ws         = 2

    !--- Tillering
    method_pop        = tillermet

    !--- Hourly Hour temperature
    a_pl        = 1.607 !Calibrated for Sao Paulo State   (original constants from Parton and Logan paper = 2.000)
    b_pl        = 2.762 !Calibrated for Sao Paulo State   (original constants from Parton and Logan paper = 2.200)
    c_pl        = 1.179 !Calibrated for Sao Paulo State   (original constants from Parton and Logan paper = -0.17)

    !----------------------------------!
    !--- Crop States Initialization ---!
    !----------------------------------!

    !--- Leaf dry weight at end of life-spam of a leaf grown under optimun conditions [g]
    max_lf_dw       = mla / sla_sam ! Use this while the model considers fixed SLA (PIT)

    !--- Initial dry mass [t ha-1] in planting date (using flat bed method for planting)
    !--- Assuming stalks with 1.5 kg and 2 meters
    init_stalkfw        = 1.5d0 !kg
    init_stalkht        = 2.d0  !m
    nstalks_planting    = 2.d0  !#
    ini_nstk            = 5. * 1. / (rowsp / 100.) ! plants m-2 - Assuming 5 emerged stems per 1 linear meter (20 cm between each other)
    tilleragefac        = 1.

    if(ratoon)then

        !-----------------!
        !--- Ratooning ---!
        !-----------------!

        pltype = 'Ratoon'

        !--- Initializing phytomer profile
        phprof    = 0.d0

        initcropdepth = min(init_plantdepth_ratoon, plantdepth)
        if(bottom(1) .ge. initcropdepth)then
            initcropdepth = bottom(1) + 0.01 !Ensure that the ratoon depth shoot is below first soil layer
        endif

        nseason = seqnow

        !--- Check root dry weight left from last season
        if(nseason .eq. 1) then

            !--- If ratoonng and 1st season: Assume Laclau & Laclau (2009) Root dryWeight at maturity ~ 120 g m-2
            dw_rt   = max_rt_dw * (1.e4 / 1.e6) ! [ton ha-1]

            !--- Substrates for initial growth - Use same as plant cane when ratoon is the first season
            dw_it_BG    = (init_stalkfw / init_stalkht*nstalks_planting) &
            &/ (rowsp/100.) / 1.e3 * 1.e4 ! [ton ha-1]

            !--- Considering 70% of water, 15% structural biomass. Substrates reserves are sucrose (13%) and hexoses (2%)
            str_it_BG   =   dw_it_BG *  0.15d0  ! 15% Fiber
            sug_it_BG   =   dw_it_BG *  0.15d0  ! 15% Sugars
            suc_it_BG   =   dw_it_BG *  0.13d0  ! 13% Sucrose
            hex_it_BG   =   dw_it_BG *  0.02d0  !  2% Hexoses
            dw_it_BG    =   dw_it_BG *  0.3d0   ! 70% Water
            dw_it       =   dw_it_BG

            !--- Fraction of roots left alive after harvesting
            ini_dw_rt   = dw_rt     *   rootleftfrac
            dw_rt       = ini_dw_rt
            dw_total    = dw_rt     +   str_it_BG   +   sug_it_BG

            !--- Roots reached in maximum depth
            rdm             = min(rdm, bottom(numlay))
            rd              = rdm
            effective_rd    = initcropdepth

            !--- root profile 
            srl  = srlmin
            
            !--- get drld
            ddw_rt = dw_rt
            call root_growth_factor(cumdenstb, ddw_rt,              &
                                  upper, bottom, SLTHICKNESS, dep,  & 
                                  rootshape, drld)
            ddw_rt = 0.d0            
            
            !--- initialize rld
            rld      = rld  +   drld
            
            cropdstage      =  'Sprout'
            cropstatus      = ' Alive'
            flcropalive     = .true.

        else

            !--- Sequential Ratooning
            !--- Last season below ground biomass will not be reseted in order to be used as initial conditions

            !--- Roots remained alive
            ini_dw_rt   = dw_rt     *   rootleftfrac
            dw_rt       = ini_dw_rt
            rld         = rld       *   rootleftfrac
            dw_total    = dw_rt

            !--- Maximum Root depth [cm]
            !--- Note that the last season root depth is not modified
            rdm             = min(rdm, bottom(numlay))
            effective_rd    = initcropdepth

            !--- Crop Stage
            if(flcropalive) cropdstage  = 'Sprout'

        endif

    else

        !------------------!
        !--- Plant Cane ---!
        !------------------!

        pltype = 'PlCane'

        !Initial Partitionioning factors
        rgpf = 1.00
        sgpf = 0.d0
        lgpf = 0.d0

        !--- Initializing phytomer profile
        phprof    = 0.d0

        !--- Initial Crop depth as same as the planting depth [cm]
        initcropdepth = plantdepth
        
        !--- Substrates reserve for before emergence is considered as sugars content remaining in the chopped stalks
        dw_it_BG    =   (init_stalkfw / init_stalkht * nstalks_planting) &
        &   / (rowsp/100.) / 1.e3 * 1.e4 ! [ton ha-1]

        !--- Considering 70% of water, 15% structural biomass. Substrates reserves are sucrose (13%) and hexoses (2%)
        str_it_BG   =   dw_it_BG *  0.15d0  ! 15% Fiber
        sug_it_BG   =   dw_it_BG *  0.15d0  ! 15% Sugars
        suc_it_BG   =   dw_it_BG *  0.13d0  ! 13% Sucrose
        hex_it_BG   =   dw_it_BG *  0.02d0  !  2% Hexoses
        dw_it_BG    =   dw_it_BG *  0.3d0   ! 70% Water
        dw_it       =   dw_it_BG

        !--- Fraction of roots left alive after harvesting
        ini_dw_rt   = 0.d0  ! No Roots in plant cane initial conditions
        rld         = 0.d0
        dw_rt       = ini_dw_rt
        dw_total    = str_it_BG   +   sug_it_BG

        !--- Crop Depth [cm]
        rdm             = min(rdm, bottom(numlay))
        effective_rd    = initcropdepth
        rd              = initcropdepth

        !--- Crop Stage
        cropdstage      =  'Sprout'
        cropstatus      = ' Alive'
        flcropalive     = .true.

    endif

    !--- Biomass and Aerial Conditions
    dw_aerial   			=   0.d0
    dw_lf       			=   0.d0
    dw_lf_BG    			=   0.d0
    dw_lf_AG    			=   0.d0
    dw_it_AG    			=   0.d0
    dw_it       			=   dw_it_AG + dw_it_BG
    str_it_AG   			=   0.d0
    sug_it_AG   			=   0.d0
    wat_it_AG   			=   0.d0
    fw_it_AG    			=   0.d0
    suc_it_AG   			=   0.d0
    hex_it_AG   			=   0.d0
    nstk					=	0.d0
    lai         			=   0.d0
    lai_ass     			=   0.d0
    stk_h					=	0.d0
    diac        			=   0.d0
    diacsoil    			=   0.d0
    diacem      			=   0.d0
    diacsoilem  			=   0.d0
    nstk_now    			=   ini_nstk
    n_lf_tiller 			=   0.d0
    sug_cont    			=   0.d0

    !--- Counters
    n_lf_dead               = 0
    n_lf_dead_AG            = 0
    n_lf_dead_BG            = 0
    n_lf_alive_juveni	      = 1
    n_lf_alive_dewlap       = 0
    n_lf_AG_dewlap          = 0
    n_ph_AG                 = 0
    n_lf_alive_AG		      = 0
    n_lf_alive_juveni_AG    = 0
    n_it_AG                 = 0
    n_ph_BG                 = 1
    n_lf_BG                 = 1
    n_lf_alive_BG           = 1
    n_lf_alive_juveni_BG    = 1
    n_ph		              = 1
    n_lf_alive              = 1
    n_it                    = 1
    n_it_BG                 = 1
    n_lf                    = 1
    n_lf_AG                 = 0

    if(.not. (aint(nstk_now) .eq. nstk_now))then
        !--- Increase one tiller to account for decimals
        atln_now    = aint(nstk_now) + 1
    else
        atln_now    = aint(nstk_now)
    endif

    !--- Shared Sugars among below ground internodes
    shared_it_sug_BG =  (sug_it_BG * (1.e6/1.e4) / &
    & (ini_nstk * tilleragefac)) / n_it_BG
    
    shared_it_str_BG =  (str_it_BG * (1.e6/1.e4) / &
    & (ini_nstk * tilleragefac)) / n_it_BG

    !--- Update profile total sugars and biomass [g]
    phprof(1:n_ph,50) = shared_it_sug_BG + shared_it_str_BG
    phprof(1:n_ph,51) = shared_it_str_BG
    phprof(1:n_ph,52) = shared_it_sug_BG
    phprof(1:n_ph,53) = shared_it_sug_BG * frac_hex_BG
    phprof(1:n_ph,54) = shared_it_sug_BG * frac_suc_BG
    phprof(1:n_ph,14) = max_it_dw_BG
    phprof(1:n_ph,11) = kmr_stor
    phprof(1:n_ph,13) = q10_stor

    !--- Flags
    fl_use_reserves     = .true.
    flemerged           = .false.
    fl_stalk_emerged    = .false.
    fl_shed_leaf        = .false.
    fl_it_visible       = .false.
    fl_hasleaf          = .false.
    fl_appear_leaf      = .false.
    fl_tiller_increase  = .true.
    fl_tiller_peaked    = .false.
    fl_tiller_decrease  = .false.
    fl_tiller_stop      = .false.
    fl_it_AG            = .false.
    fl_lf_AG            = .false.
    fl_lf_alive(1:n_ph) = .true.

    poppeak_lt          = 0.d0
    chudec_lt           = 0.d0
    dnstk_dead_rate     = 0.d0
    chumat_lt           = 0.d0

    !--- Initial root dry weight in g m-2
    ini_dw_rt   =   ini_dw_rt   *   (1.e6 / 1.e4)

    !--- Initial Crop Depth [cm]
    shootdepth          = initcropdepth
    diac_at_emergence   = 0.d0

    !--- Resources used for emergence (reset plant memory)
    res_used_emerg      = 0.d0

    !--- Roots cumulative density from RLD (Required by SWAP)
    if (cumdenstb .eq. 0) then
        call root_cumdens(numlay,rld,upper,bottom,cumdens)
    endif
    
    !--------------------!
    !--- Output files ---!
    !--------------------!

    !--- File i/o units
    outp        =   901
    outd        =   902
    outdph      =   903
    outdpa      =   904
    outpfac     =   905
    outstres    =   906
    outds       =   907
    outcumd     =   908

    if(flinit_file)then
        !--- open output files
        call output_samuca(  2, &
        &                    project, &
        &                    outp, &
        &                    outd, &
        &                    outdph, &
        &                    outdpa, &
        &                    outpfac, &
        &                    outstres, &
        &                    outds, &
        &                    outcumd, &
        &                    writedetphoto, &
        &                    writedcrop, &
        &                    writehead, &
        &                    detailedsoil, &
        &                    writecumdens)
    endif

    return

2000 continue

    !-----------------------!
    !--- Potential rates ---!
    !-----------------------!

    !--- Not in use by SAMUCA.
    !--- To run potential conditions, turn-off the Limiting Factors -> potential_growth = .true.

    return

3000 continue

    !-------------------------------------!
    !--- Time-Step Rate Initialization ---!
    !-------------------------------------!

    !--- Link with SWAP variables
    srad    =   rad * 1.e-6
    tmax    =   tmx
    tmin    =   tmn
    tmed    =   (tmax + tmin) / 2.d0
    eop     =   ptra * 10.d0
    trwup   =   tra
    doy     =   daynr
    das     =   daycum
    dap     =   daycrop
    year    =   iyear
    lat_sim =   lat

    !--- Crop Step-Rates
    di							= 0.d0
    disoil						= 0.d0
    drdepth						= 0.d0
    dshootext_BG				= 0.d0
    rgf     					= 0.d0
    dw_rt_prof                  = 0.d0
    drld						= 0.d0
    drld_dead					= 0.d0
    dphy_stimuli				= 0.d0
    dla_gain_ref_till           = 0.d0
    dlai_gain					= 0.d0
    dlai_dead					= 0.d0
    dlai_shed					= 0.d0
    dlai_gain_appear            = 0.d0
    ddw_lf_appear               = 0.d0
    per							= 0.d0
    per_hour					= 0.d0
    dnstk						= 0.d0
    ddw_rt         				= 0.d0
    ddw_lf         				= 0.d0
    ddw_lf_BG                   = 0.d0
    ddw_lf_AG                   = 0.d0
    ddw_it         				= 0.d0
    ddw_it_AG      				= 0.d0
    dstr_it_AG     				= 0.d0
    dsug_it_AG     				= 0.d0
    ddw_it_BG      				= 0.d0
    dstr_it_BG     				= 0.d0
    dsug_it_BG     				= 0.d0
    dwat_it_AG     				= 0.d0
    ddw_lf_shed    				= 0.d0
    dsubsres       				= 0.d0
    ddw_rt_dead    				= 0.d0
    ddw_lf_dead    				= 0.d0
    ddw_it_dead    				= 0.d0
    ddw_it_AG_dead 				= 0.d0
    dstr_it_AG_dead				= 0.d0
    dsug_it_AG_dead				= 0.d0
    ddw_it_BG_dead 				= 0.d0
    dstr_it_BG_dead				= 0.d0
    dsug_it_BG_dead				= 0.d0
    dwat_it_AG_dead				= 0.d0

    !--- Respiration Rates and Sink Strenght
    tot_gresp_lf 				= 0.d0
    tot_mresp_lf 				= 0.d0
    tot_dw_ss_lf 				= 0.d0
    tot_gresp_it_AG 			= 0.d0
    tot_mresp_it_AG 			= 0.d0
    tot_dw_ss_it_AG 			= 0.d0
    tot_gresp_it_BG 			= 0.d0
    tot_mresp_it_BG 			= 0.d0
    tot_dw_ss_it_BG 			= 0.d0
    tot_gresp_rt				= 0.d0
    tot_mresp_rt				= 0.d0
    dw_ss_rt					= 0.d0
    dtlfss						= 0.d0
    dtitss						= 0.d0
    dtitss_AG					= 0.d0
    dtitss_BG					= 0.d0
    dtrtss						= 0.d0
    dtcrss						= 0.d0
    tot_gresp_it				= 0.d0
    tot_mresp_it				= 0.d0
    tot_dw_ss_it				= 0.d0
    tot_gresp_crop 				= 0.d0
    tot_mresp_crop 				= 0.d0
    tot_dw_ss_crop 				= 0.d0
    dr_rtss 					= 0.d0
    dr_lfss 					= 0.d0
    dr_itss 					= 0.d0
    frac_AG   					= 0.d0
    frac_BG   					= 0.d0
    rel_ss_lf_phy				= 0.d0
    rel_ss_it_phy				= 0.d0
    dw_ss_lf					= 0.d0
    gresp_lf					= 0.d0
    mresp_lf					= 0.d0
    dw_ss_it					= 0.d0
    gresp_it					= 0.d0
    mresp_it					= 0.d0

    !--- Substrates Assimilated
    dtg							= 0.d0
    dtga                        = 0.d0
    frac_li                     = 0.d0
    li                          = 0.d0
    Acanopy				        = 0.d0
    Qleaf				        = 0.d0
    incpar				        = 0.d0
    photo_layer_act				= 0.d0
    avail_subs_crop				= 0.d0
    dtg_avail_rt    			= 0.d0
    dtg_avail_lf    			= 0.d0
    dtg_avail_it    			= 0.d0
    dtg_avail_it_BG 			= 0.d0
    dtg_avail_it_AG 			= 0.d0
    subsres_avail_rt    		= 0.d0
    subsres_avail_lf    		= 0.d0
    subsres_avail_it    		= 0.d0
    subsres_avail_it_BG 		= 0.d0
    subsres_avail_it_AG 		= 0.d0
    sup_ratio_lf				= 0.d0
    supply_rate_lf				= 0.d0
    supply_used_lf				= 0.d0
    supply_used_mresp_lf		= 0.d0
    supply_used_gresp_lf		= 0.d0
    supply_used_dw_lf			= 0.d0
    reserves_used_mresp_lf		= 0.d0
    maintenance_factor_lf		= 1.d0
    reduc_growth_factor_lf		= 1.d0
    sup_ratio_it				= 0.d0
    supply_rate_it				= 0.d0
    supply_used_it				= 0.d0
    supply_used_mresp_it		= 0.d0
    supply_used_gresp_it		= 0.d0
    supply_used_dw_it			= 0.d0
    reserves_used_mresp_it		= 0.d0
    maintenance_factor_it		= 1.d0
    reduc_growth_factor_it		= 1.d0
    sup_ratio_it_BG				= 0.d0
    supply_rate_it_BG			= 0.d0
    supply_used_it_BG			= 0.d0
    supply_used_mresp_it_BG		= 0.d0
    supply_used_gresp_it_BG		= 0.d0
    supply_used_dw_it_BG		= 0.d0
    reserves_used_mresp_it_BG	= 0.d0
    maintenance_factor_it_BG	= 1.d0
    reduc_growth_factor_it_BG	= 1.d0
    sup_ratio_it_AG				= 0.d0
    supply_rate_it_AG			= 0.d0
    supply_used_it_AG			= 0.d0
    supply_used_mresp_it_AG		= 0.d0
    supply_used_gresp_it_AG		= 0.d0
    supply_used_dw_it_AG		= 0.d0
    reserves_used_mresp_it_AG	= 0.d0
    maintenance_factor_it_AG	= 1.d0
    reduc_growth_factor_it_AG	= 1.d0
    sup_ratio_rt				= 0.d0
    supply_rate_rt				= 0.d0
    supply_used_rt				= 0.d0
    supply_used_mresp_rt		= 0.d0
    supply_used_gresp_rt		= 0.d0
    supply_used_dw_rt			= 0.d0
    reserves_used_mresp_rt		= 0.d0
    maintenance_factor_rt		= 1.d0
    reduc_growth_factor_rt		= 1.d0
    supply_used_crop		    = 0.d0
    supply_used_mresp_crop	    = 0.d0
    supply_used_gresp_crop	    = 0.d0
    supply_used_dw_crop		    = 0.d0
    reserves_used_mresp_crop    = 0.d0
    maintenance_factor_crop	    = 1.d0
    reduc_growth_factor_crop    = 1.d0
    dtg_avail_lf_ref_till       = 0.d0
    dtg_avail_it_ref_till       = 0.d0
    dtg_avail_it_BG_ref_till    = 0.d0
    dtg_avail_it_AG_ref_till    = 0.d0
    subsres_avail_lf_ref_till   = 0.d0
    subsres_avail_it_ref_till   = 0.d0
    subsres_avail_it_BG_ref_till= 0.d0
    subsres_avail_it_AG_ref_till= 0.d0
    dtlfss_ref_till             = 0.d0
    dtitss_ref_till             = 0.d0
    dtitss_BG_ref_till          = 0.d0
    dtitss_AG_ref_till          = 0.d0
    dtg_avail_lf_phy          	= 0.d0
    subsres_avail_lf_phy      	= 0.d0
    dtlfss_phy                	= 0.d0
    mresp_lf_phy              	= 0.d0
    gresp_lf_phy              	= 0.d0
    dw_ss_lf_phy              	= 0.d0
    sup_ratio_lf_phy          	= 0.d0
    supply_rate_lf_phy        	= 0.d0
    supply_used_lf_phy        	= 0.d0
    supply_used_mresp_lf_phy  	= 0.d0
    supply_used_gresp_lf_phy  	= 0.d0
    supply_used_dw_lf_phy     	= 0.d0
    reserves_used_mresp_lf_phy	= 0.d0
    maintenance_factor_lf_phy 	= 1.d0
    reduc_growth_factor_lf_phy	= 1.d0
    dtg_avail_it_phy          	= 0.d0
    subsres_avail_it_phy      	= 0.d0
    dtitss_phy                	= 0.d0
    mresp_it_phy              	= 0.d0
    gresp_it_phy              	= 0.d0
    dw_ss_it_phy              	= 0.d0
    sup_ratio_it_phy          	= 0.d0
    supply_rate_it_phy        	= 0.d0
    supply_used_it_phy        	= 0.d0
    supply_used_mresp_it_phy  	= 0.d0
    supply_used_gresp_it_phy  	= 0.d0
    supply_used_dw_it_phy     	= 0.d0
    reserves_used_mresp_lf_phy	= 0.d0
    maintenance_factor_it_phy 	= 1.d0
    reduc_growth_factor_it_phy	= 1.d0
    it_struc_pfac_rate			= 0.d0
    dstr_it_phy					= 0.d0
    dsug_it_phy					= 0.d0
    exc_dtg_lf      			= 0.d0
    exc_dtg_it      			= 0.d0
    exc_dtg_rt      			= 0.d0
    dtot_str_dw_ref_till		= 0.d0
    suc_it_AG_ref_till	        = 0.d0
    hex_it_AG_ref_till	        = 0.d0
    suc_it_BG_ref_till	        = 0.d0
    hex_it_BG_ref_till	        = 0.d0
    shared_it_sug_BG            = 0.d0

    !--- Crop Stress factors
    swfacp						= 1.d0
    swface						= 1.d0
    swfact						= 1.d0
    swfacf						= 1.d0
    agefactor_per				= 1.d0
    agefactor_rue				= 1.d0
    pho_fac_co2					= 1.d0
    tempfac_h_per				= 1.d0
    tempfac_pho					= 1.d0
    tempfac_per					= 1.d0
    amaxfbfac					= 1.d0
    agefactor_amax              = 1.d0

    !--- Phytomer Rates
    phprof(1: 100, 2) 		= 0.d0  ! Leaf Sink strenght
    phprof(1: 100, 3) 		= 0.d0  ! Allocated Leaf biomass
    phprof(1: 100, 4) 		= 0.d0  ! Leaf area rate
    phprof(1: 100, 7) 		= 0.d0  ! Internode Sink Strength dw rate g d-1
    phprof(1: 100,21) 		= 0.d0 	! Internode Growth Respiration
    phprof(1: 100,22) 		= 1.d0 	! Maintenance Respiration Factor (0-1) 1 =  is maintenance is ok
    phprof(1: 100,23) 		= 0.d0 	! dLength (cm)
    phprof(1: 100,25) 		= 0.d0 	! mresp leaf
    phprof(1: 100,26) 		= 0.d0 	! gresp leaf
    phprof(1: 100,27) 		= 0.d0 	! dw ss leaf
    phprof(1: 100,28) 		= 1.d0 	! sup_ratio_lf_phy
    phprof(1: 100,29) 		= 0.d0 	! supply_rate_lf
    phprof(1: 100,30) 		= 0.d0 	! sup_ratio_lf_phy
    phprof(1: 100,31) 		= 0.d0 	! supply_used_mresp_lf
    phprof(1: 100,32) 		= 0.d0 	! supply_used_gresp_lf
    phprof(1: 100,33) 		= 0.d0 	! supply_used_dw_lf
    phprof(1: 100,34) 		= 1.d0 	! maintenance_factor_lf
    phprof(1: 100,35) 		= 1.d0 	! reduc_growth_factor_lf
    phprof(1: 100,36) 		= 0.d0 	! mresp internode
    phprof(1: 100,37) 		= 0.d0 	! gresp internode
    phprof(1: 100,38) 		= 0.d0 	! dw ss internode
    phprof(1: 100,39) 		= 0.d0 	! supply_rate_it
    phprof(1: 100,40) 		= 1.d0 	! sup_ratio_it_phy
    phprof(1: 100,41) 		= 0.d0 	! supply_used_it
    phprof(1: 100,42) 		= 0.d0 	! supply_used_mresp_it
    phprof(1: 100,43) 		= 0.d0 	! supply_used_gresp_it
    phprof(1: 100,44) 		= 0.d0 	! supply_used_dw_it
    phprof(1: 100,45) 		= 1.d0 	! maintenance_factor_it
    phprof(1: 100,46) 		= 1.d0 	! reduc_growth_factor_it
    phprof(1: 100,47) 		= 0.d0 	! Internode dry weigth rate [g dt-1]
    phprof(1: 100,48) 		= 0.d0 	! Internode structural dry weigth rate [g dt-1]
    phprof(1: 100,49) 		= 0.d0 	! Internode total sugars rate [g dt-1]
    phprof(1: 100,55)       = 0.d0  ! Leaf Age rate [dCdays]
    phprof(1: 100,56)       = 0.d0  ! Phytomer Age rate [dCdays]
    phprof(1: 100,57)       = 0.d0  ! Internode Age rate [dCdays]

    !--- Check if crop is alive
    if(.not. flcropalive) return

    !------------------------!
    !--- DEFINING FACTORS ---!
    !------------------------!

    !-----------------------!
    !--- Solar Radiation ---!
    !-----------------------!

    !--- Fraction of Solar Radiation Crop Can Actively Use
    !--- Photosynthetically Active Radiation (PAR) - Assume as 50% of total incoming radiation flux
    par_rad = srad * 0.5d0

    !--- Canopy light interception fraction (Beer Law)
    li      = 1.d0 - exp(-k_can * lai)

    !--- Canopy intercepted PAR [MJ]
    can_ipar = par_rad * li

    !-------------------!
    !--- Temperature ---!
    !-------------------!

    !--- Temperature stress on photosynthesis
    tempfac_pho      = temperature_factor(tmed, tb0pho, tb1pho,tb2pho, &
    & tbfpho) ! Photosynthesis

    !--- Temperature stress factor on expansioning
    tempfac_per      = min(1.,max(0.,tmed - tbper) / &
    & (tbMax_per - tbper))

    !--- Hourly Temperature (PL model)
    call TempHour_samuca(tmax,tmin,doy,lat_sim,a_pl,b_pl,c_pl,thour)

    !--- Hourly Plant Extension Temperature Factor
    do hour = 1, 24
        tempfac_h_per(hour)   = min(1.d0, max(0.d0, thour(hour) - tbper) &
        & / (tbMax_per - tbper))
    enddo

    !-------------------------------------!
    !--- Atmospheric CO2 Concentration ---!
    !-------------------------------------!

    !--- Following Mathew Jones and Abraham Singels (https://doi.org/10.1016/j.eja.2017.12.009)
    !--- No effect on C4 Photosynthesis after 270 ppm - Higher Water Use Efficiency is believed to be the reason of increased biomass gain under increased CO2 (included in ptrans())
    if(co2 .gt. co2_pho_res_end)then
        !--- Optimun CO2 conditions
        pho_fac_co2 =   1.d0
    else if(co2 .lt. co2_pho_res_ini)then
        !--- Extreme Low CO2 conditions (No photosynthesis)
        pho_fac_co2 =   0.d0
    else
        !--- Transient concentration
        pho_fac_co2 = (co2_pho_res_end - co2) / &
        & (co2_pho_res_end - co2_pho_res_ini)
    endif

    !-----------------------!
    !--- Genotype traits ---!
    !-----------------------!

    !--- Aging factor to account for reduced growth due to crop aging
    !--- Necessary to include effects of the Reduced Growth Phenomena (RGP) still not well understood,
    !--- or reduced turgor pressure on top parts in planting extension.
    agefactor_rue = exp(agefactor_fac_rue * diacem)
    agefactor_rue = min(1.d0, agefactor_rue)

    !--- Max assimimilation reduction
    agefactor_amax = exp(agefactor_fac_amax * diacem)
    agefactor_amax = min(1.d0, agefactor_amax)

    !--- Age reduction factor for dper, based on N. G. Inman-Bamber et al. 2008 Australian Journal of Agricultural Research, Fig.3
    agefactor_per = exp(agefactor_fac_per * diacem)
    agefactor_per = min(1.d0, agefactor_per)

    !------------------------!
    !--- LIMITING FACTORS ---!
    !------------------------!

    if(.not. fl_potential)then

        !--- Water Stress ---!
        call waterstress(   2, &
        & ndws, &
        & ndews, &
        & eop, &
        & trwup, &
        & rwuep1, &
        & rwuep2 , &
        & t_max_ws_pho, &
        & t_mid_ws_pho, &
        & t_min_ws_pho, &
        & t_max_ws_exp, &
        & t_mid_ws_exp, &
        & t_min_ws_exp, &
        & t_max_ws_til, &
        & t_mid_ws_til, &
        & t_min_ws_til, &
        & t_max_ws_fpf, &
        & t_mid_ws_fpf, &
        & t_min_ws_fpf, &
        & threshews, &
        & swfacp, &
        & swface, &
        & swfact, &
        & swfacf)

        if(swfacp .lt. 1.d0 .and. flemerged)then
            fl_use_reserves = .false. ! do not use reserves when crop is stressed-out
        endif


        !--- Soil Nutrients ---!

        !--- (PIT) ---!

    endif

    !------------------------!
    !--- REDUCING FACTORS ---!
    !------------------------!

    !--- (PIT) ---!


    !------------------------!
    !--- CROP DEVELOPMENT ---!
    !------------------------!

    !----------------!
    !--- Age Rate ---!
    !----------------!
    if(usetsoil)then

        !----------------------------!
        !--- Use Soil Temperature ---!
        !----------------------------!

        !--- Retrive soil temperature for each layer
        tsoil_lay   = 0.d0
        sl          = 1
        n_sub_sl    = 1
        do sub_sl = 1, numnod

            tsoil_lay(sl)   =  tsoil_lay(sl) +  tsoil(sub_sl) *  &
            & dz(sub_sl) / slthickness(sl)

            n_sub_sl = n_sub_sl + 1
            if(n_sub_sl .gt. ncomp(sl))then
                n_sub_sl = 1
                sl       = sl + 1
            endif
        enddo

        if(mulcheffect)then
            !--- Mulch effect when mulch is present (skip the first layer temperature (mulch))
            !--- average soil temperature in plant depth soil layers (weighted mean)
            soiltemperature = 0.d0
            do sl = 1, numlay
                if(initcropdepth .gt. upper(sl)) then

                    if(initcropdepth .gt. bottom(sl)) then
                        soiltemperature = soiltemperature + &
                        & tsoil_lay(sl+1) * slthickness(sl) / initcropdepth
                    else
                        soiltemperature = soiltemperature + &
                        & tsoil_lay(sl+1) * (initcropdepth - bottom(sl-1))  / initcropdepth
                    endif
                endif
            enddo
        else
            !--- use soil temperature but without mulch effect (bare soil)
            !--- average soil temperature in plant depth soil layers (weighted mean)

            soiltemperature = 0.d0
            do sl = 1, numlay
                if(initcropdepth .gt. upper(sl)) then
                    if(initcropdepth .ge. bottom(sl)) then
                        soiltemperature = soiltemperature +  &
                        & tsoil_lay(sl) * slthickness(sl) / initcropdepth
                    else
                        soiltemperature = soiltemperature +  &
                        & tsoil_lay(sl) * (initcropdepth - bottom(sl-1))  / initcropdepth
                    endif
                endif
            enddo
        endif

        !--- Degree-days using soil and air temperatures
        disoil  = min(max(0.d0, soiltemperature - tb)   ,tbm - tb)
        diair   = min(max(0.d0, tmed - tb)              ,tbm - tb)

        !--- Use soil temperature to compute crop age until stalks arises (Appical meristems is the sensor)
        if(.not. fl_stalk_emerged) then

            !--- Computing crop age (degree-days) based on soil temperature
            di      = disoil

            if(flemerged)then
                !--- Leaves emerges prior to stalks
                dileaf = diair
            else
                !--- Before emergence leaves (shoots) are below ground
                dileaf  = disoil
            endif
        else

            !--- Computing crop age (degree-days) based on air temperature
            di      = diair
            dileaf  = diair
        endif

    else

        !--------------------------------!
        !--- Not Use Soil Temperature ---!
        !--------------------------------!

        !--- Considering soil temperature equal as air temperature
        soiltemperature = tmed

        !--- Computing crop age (degree-days) based on air temperature
        di      = min(max(0.d0, tmed - tb), tbm - tb)
        diair   = di
        disoil  = di
        dileaf  = di

    endif

    !------------------------!
    !--- Phytomer Stimuli ---!
    !------------------------!

    !--- Stimuli rate [phy/dt]
    dphy_stimuli    = (1.d0 / plastochron) * di


    if(flemerged)then

        !-----------------!
        !--- Tillering ---!
        !-----------------!

        select case(method_pop)

        case(1)

            !------------------------------------!
            !--- Tillering Rate ~ Thermal Age ---!
            !------------------------------------!

            !--- Daily initial tillers numbers rate
            if(diacsoil .lt. (chupeak + diac_at_emergence)) then
                !--- Initial tiller grow
                dnstk = ((poppeak-ini_nstk)/(chupeak)) * disoil

            elseif(diacsoil .ge. (chupeak + diac_at_emergence) .and.  &
                & diacsoil .lt. (chudec + diac_at_emergence)) then
                !--- tillering peak
                dnstk = 0.

            elseif(diacsoil .ge. (chudec + diac_at_emergence) .and.  &
                & diacsoil .lt. (chumat + diac_at_emergence)) then
                !--- reduction phase to mature (abortion of tillers)
                dnstk = (-(poppeak - popmat) / ((chumat)-(chudec)))*disoil

            elseif(diacsoil .ge. (chumat+diac_at_emergence)) then
                !--- late stable tiller population
                dnstk = 0.
            endif

        case(2)

            !------------------------------------------------------!
            !--- Tillering Rate ~ Thermal Age + Solar Radiation ---!
            !------------------------------------------------------!


            !--- Dead leaf position below the living leaves profile
            lf_dpos     = n_lf_alive_AG + 1

            !--- Dead LAI, where the fdeadlf is the fraction of blades of attached dead leaves
            dead_lai    = sum(phprof(lf_dpos : (lf_dpos +  &
            & aint(nsenesleaf_effect)), 5)) * nstk_now *  &
            & tilleragefac * 1.e-4 * fdeadlf

            !--- Modified LAI
            laimod = lai + dead_lai

            !--- Transmitted Light Through Canopy
            lt     = exp(-k_can * laimod)

            !--- Check Tillering States
            if(lt .lt. ltthreshold .and. .not. fl_tiller_peaked)then
                !--- Fisrt time reaching tillering peak
                fl_tiller_peaked    = .true.
                fl_tiller_stop      = .true.

                !--- Store the peak of population
                poppeak_lt          = nstk_now
                chudec_lt           = diacsoil
                chumat_lt           = chudec_lt + tt_chumat_lt

                !--- Tillering dead rate [till cdays-1]
                dnstk_dead_rate     = (popmat - poppeak_lt) /  &
                & (chumat_lt - chudec_lt)

            else if(lt .lt. ltthreshold .and. fl_tiller_peaked)then
                !--- Decrease tillering
                fl_tiller_decrease  = .true.
                fl_tiller_stop      = .false.

                !--- Update Tillering dead rate [till cdays-1]
                if(diacsoil .ge. chumat_lt)then
                    dnstk_dead_rate = 0.d0
                else
                    dnstk_dead_rate     = (popmat - nstk_now) /  &
                    & (chumat_lt - diacsoil)
                endif

            else if(lt .gt. ltthreshold .and. fl_tiller_peaked)then
                !--- Don't let increase tillering after peak has reached (Unless lodging happens and resets fl_tiller_peaked)
                fl_tiller_stop      = .true.
            else
                !--- Increase tillering
                fl_tiller_increase  = .true.
                fl_tiller_stop      = .false.
            endif

            !--- No change in tillering
            if(fl_tiller_stop)then
                fl_tiller_increase  = .false.
                fl_tiller_decrease  = .false.
            endif

            !--- Tillering rate
            if(fl_tiller_increase)then
                !--- Increase number of tiller
                dnstk = disoil / tillochron
            elseif(fl_tiller_decrease)then
                !--- Decrease number of tiller
                dnstk = dnstk_dead_rate * disoil
            else
                !--- Stabilize plant population
                dnstk = 0.d0
            endif

        end select

        !--- Dead Tissues rate due to tiller senescence
        if(dnstk .lt. 0.d0)then

            !--- Dead Biomass rates [ton ha-1]
            ddw_lf_dead         =   tiller_senes(dw_lf, &
            & nstk_now, dnstk, tilleragefac, tillerageprof, atln)
            
            ddw_it_dead         =   tiller_senes(dw_it,  &
            & nstk_now, dnstk, tilleragefac, tillerageprof, atln)
            
            ddw_it_AG_dead      =   tiller_senes(dw_it_AG,  &
            & nstk_now, dnstk, tilleragefac, tillerageprof, atln)
            
            dstr_it_AG_dead     =   tiller_senes(str_it_AG,  &
            & nstk_now, dnstk, tilleragefac, tillerageprof, atln)
            
            dsug_it_AG_dead     =   tiller_senes(sug_it_AG,  &
            & nstk_now, dnstk, tilleragefac, tillerageprof, atln)
            
            ddw_it_BG_dead      =   tiller_senes(dw_it_BG,  &
            & nstk_now, dnstk, tilleragefac, tillerageprof, atln)
            
            dstr_it_BG_dead     =   tiller_senes(str_it_BG,  &
            & nstk_now, dnstk, tilleragefac, tillerageprof, atln)
            
            dsug_it_BG_dead     =   tiller_senes(sug_it_BG,  &
            & nstk_now, dnstk, tilleragefac, tillerageprof, atln)
            
            dwat_it_AG_dead     =   tiller_senes(wat_it_AG,  &
            & nstk_now, dnstk, tilleragefac, tillerageprof, atln)

            !--- Dead Leaf Area Index [m2 m-2]
            dlai_dead = ddw_lf_dead * (1.e6/1.e4) * sla_sam / 1.e4

        endif

    endif

    !--- Leaf shedding rate
    if(fl_shed_leaf)then

        !--- Senesced leaf dry weight and area
        dw_lf_shed_phy      =   phprof(n_lf_alive + 1, 6)
        la_lf_shed_phy      =   phprof(n_lf_alive + 1, 5)
        nstk_at_appearance  =   phprof(n_lf_alive + 1, 10)
        fl_lf_AG_phy        =   fl_lf_AG(n_lf_alive + 1)

        !--- Shed DW Leaf rate [ton ha-1]
        ddw_lf_shed = dw_lf_shed_phy * nstk_now * tilleragefac *  &
        & (1.e4/1.e6) * min(1.d0, nstk_at_appearance / nstk_now)

        !--- Shed Leaf Area Index [m2 m-2]
        dlai_shed   = la_lf_shed_phy * nstk_now * tilleragefac /  &
        & 1.e4   * min(1.d0, nstk_at_appearance / nstk_now)

        !--- Update dead leaves counter
        n_lf_dead   =   n_lf_dead   + 1

        if(fl_lf_AG_phy)then
            !--- Above ground leaf
            n_lf_dead_AG    =   n_lf_dead_AG    + 1
        else
            !--- Below ground leaf
            n_lf_dead_BG    =   n_lf_dead_BG    + 1
        endif

        !--- Leaf is shed, update flag
        fl_shed_leaf    =   .false.

        !--- Update leaf status
        fl_lf_alive(n_lf_alive + 1) = .false.

    endif

    !--- Leaf appearance
    if(fl_appear_leaf)then

        !--- Leaf Area Index Gain [m2 m-2]
        dlai_gain_appear    = phprof(1, 5) * nstk_now *  &
        & tilleragefac / 1.e4

        !--- Leaf Dry Weight Gain [ton ha-1]
        ddw_lf_appear       = phprof(1, 6) * nstk_now *  &
        & tilleragefac * 1.e4/1.e6

        !--- Leaf appeared, update flag
        fl_appear_leaf = .false.

    endif

    !---------------------!
    !--- Sink Strength ---!
    !---------------------!

    !--- Phytomer Level
    do phy = 1, n_ph

        !--- Check which temperature to use (soil or air) based on phytomer position

        if(fl_it_AG(phy))then
            !--- Above ground
            diphy   = di
            t_mresp = tmed
        else
            !--- Below ground
            diphy   = disoil
            t_mresp = soiltemperature
        endif

        !---------------------------!
        !--- Flag of active leaf ---!
        !---------------------------!
        if(phy .le. n_lf_alive)then
            fl_hasleaf  = .true.
        else
            fl_hasleaf  = .false.
        endif
        !---------------------------!

        !---------------------------------!
        !--- Flag of visible internode ---!
        !---------------------------------!
        if(phy .gt. n_lf_it_form)then
            fl_it_visible   = .true.
        else
            fl_it_visible   = .false.
        endif
        !---------------------------------!

        if(fl_hasleaf) then

            !------------!
            !--- Leaf ---!
            !------------!

            !--- Leaf Age [Cdays]
            age_lf_phy      = phprof(phy, 1)

            !--- Leaf Dry Weight [g]
            dw_lf_phy       = phprof(phy, 6)

            !--- Leaf Initial DW [g]
            ini_dw_lf_phy   = phprof(phy, 9)

            !--- Leaf Maintenance respiration [gCH2O]
            mresp_lf    = dw_lf_phy * (kmr_leaf * q10_leaf **  &
            & ((tmed - tref_mr) / 10.))

            !--- Leaf Dry Weight Sink Strength rate [gDW]
            dw_ss_lf    = fgrowth(1, max_lf_dw, ini_dw_lf_phy, 0.,  &
            & mid_tt_lf_growth, end_tt_lf_growth, age_lf_phy, 1.) * dileaf

            !--- Leaf Dry Weight Growth Respiration [gCH2O]
            gresp_lf    = (dw_ss_lf * (1.d0 / (1.d0 - gresp))) -  &
            & dw_ss_lf

            !--- Age rate for each living leaf
            dage_lf_phy    = max(0.d0, dileaf)

            !--- Integrate leaf sink strength
            dtlfss = dtlfss + (gresp_lf + mresp_lf + dw_ss_lf)

            !--- Update Leaf sink strength rate [gCH2O]
            phprof(phy,2)   = gresp_lf + mresp_lf + dw_ss_lf
            phprof(phy,25)  = mresp_lf                          ! mresp leaf
            phprof(phy,26)  = gresp_lf                          ! gresp leaf
            phprof(phy,27)  = dw_ss_lf                          ! dw ss leaf
            phprof(phy,55)  = dage_lf_phy

            !--- Total Growth/Maintenance Respiration and DW Sink Strength
            tot_gresp_lf = tot_gresp_lf + gresp_lf  ! [gCH2O]
            tot_mresp_lf = tot_mresp_lf + mresp_lf  ! [gCH2O]
            tot_dw_ss_lf = tot_dw_ss_lf + dw_ss_lf  ! [gDW]

        endif

        !-----------------!
        !--- Internode ---!
        !-----------------!

        !--- Internode Age [Cdays]
        age_it_phy  = phprof(phy,58)

        !--- Internode Dry Weight [g]
        dw_it_phy   = phprof(phy,50)

        !--- Maintenance Respiration
        kmr_it_phy      = phprof(phy,11)
        q10_it_phy      = phprof(phy,13)
        tref_mr_it_phy  = tref_mr

        !--- Internode Maintenance respiration [gCH2O]
        mresp_it    = dw_it_phy * (kmr_it_phy * q10_it_phy **  &
        & ((t_mresp - tref_mr_it_phy) / 10.))
        dw_ss_it    = 0.d0
        gresp_it    = 0.d0
        dage_it_phy = 0.d0

        if(fl_it_visible)then

            !--- Actively Growing Internode
            !--- Internode Dry Weight Sink Strength rate [gDW]
            max_it_dw_phy   = phprof(phy, 14)

            dw_ss_it    = fgrowth(1, max_it_dw_phy, 0., 0.,  &
            & mid_tt_it_growth, end_tt_it_growth, age_it_phy, 1.) * diphy

            !--- Internode Dry Weight Growth Respiration [gCH2O]
            gresp_it    = (dw_ss_it * (1.d0 / (1.d0 - gresp))) -  &
            & dw_ss_it

            !--- Age rate for each internode
            dage_it_phy = max(0.d0, diphy)

        endif

        !--- Integrate Internode sink strength
        if(fl_it_AG(phy))then

            !--- Above Ground
            dtitss_AG = dtitss_AG + (gresp_it + mresp_it + dw_ss_it)

            !--- Total Growth/Maintenance Respiration and DW Sink Strength
            tot_gresp_it_AG = tot_gresp_it_AG + gresp_it ! [gCH2O]
            tot_mresp_it_AG = tot_mresp_it_AG + mresp_it ! [gCH2O]
            tot_dw_ss_it_AG = tot_dw_ss_it_AG + dw_ss_it ! [gDW]

        else

            !--- Below Ground
            dtitss_BG = dtitss_BG + (gresp_it + mresp_it + dw_ss_it)

            !--- Total Growth/Maintenance Respiration and DW Sink Strength
            tot_gresp_it_BG = tot_gresp_it_BG + gresp_it ! [gCH2O]
            tot_mresp_it_BG = tot_mresp_it_BG + mresp_it ! [gCH2O]
            tot_dw_ss_it_BG = tot_dw_ss_it_BG + dw_ss_it ! [gDW]

        endif

        !--- Update phytomer profile
        phprof(phy,7)   = gresp_it + mresp_it + dw_ss_it
        phprof(phy,36)  = mresp_it                          ! mresp internode
        phprof(phy,37)  = gresp_it                          ! gresp internode
        phprof(phy,38)  = dw_ss_it                          ! dw ss internode
        phprof(phy,57)  = dage_it_phy                       ! age rate internode
        phprof(phy,56)  = max(0.d0, diphy)                  ! age rate phytomer

    enddo

    !------------!
    !--- Root ---!
    !------------!

    !--- Roots Dry Weight Sink Strength rate [gDW m-2]
    dw_ss_rt    = fgrowth(1, max_rt_dw, ini_dw_rt, 0., &
    & mid_tt_rt_growth, end_tt_rt_growth, diacsoil, 1.) * disoil
    
    tot_dw_ss_rt= dw_ss_rt

    !--- Roots Dry Weight Growth Respiration [gCH2O m-2]
    gresp_rt     = (dw_ss_rt * (1.d0 / (1.d0 - gresp))) - dw_ss_rt
    tot_gresp_rt = gresp_rt

    !--- Roots Maintenance respiration [gCH2O m-2]
    mresp_rt     = (dw_rt * 1.e6/1.e4) * (kmr_root * q10_root ** &
    & ((soiltemperature - tref_mr) / 10.))
    tot_mresp_rt = mresp_rt

    !--- Total Roots sink strength [gCH2O m-2]
    dtrtss      = gresp_rt + mresp_rt + dw_ss_rt

    !--- Use the initial plant population to scale
    !--- Upscale total substrates needed for leaf and internodes growth [gCH2O m-2] (Tiller Age factor added to account for different shoot ages in the same area)
    dtlfss      = dtlfss                    * nstk_now* tilleragefac
    dtitss      = (dtitss_BG + dtitss_AG)   * nstk_now* tilleragefac
    dtitss_BG   = dtitss_BG                 * nstk_now* tilleragefac
    dtitss_AG   = dtitss_AG                 * nstk_now* tilleragefac

    !--- Upscale total substrates needed for leaf and internodes growth/maintenance respiration and DW [gCH2O m-2, gDW m-2] (Tiller Age factor added to account for different shoot ages in the same area)
    tot_gresp_lf    = tot_gresp_lf          * nstk_now *tilleragefac
    tot_mresp_lf    = tot_mresp_lf          * nstk_now *tilleragefac
    tot_gresp_it_AG = tot_gresp_it_AG       * nstk_now *tilleragefac ! Above Ground
    tot_mresp_it_AG = tot_mresp_it_AG       * nstk_now *tilleragefac ! Above Ground
    tot_gresp_it_BG = tot_gresp_it_BG       * nstk_now *tilleragefac ! Below Ground
    tot_mresp_it_BG = tot_mresp_it_BG       * nstk_now *tilleragefac ! Below Ground
    tot_dw_ss_lf    = tot_dw_ss_lf          * nstk_now *tilleragefac
    tot_dw_ss_it_AG = tot_dw_ss_it_AG       * nstk_now *tilleragefac
    tot_dw_ss_it_BG = tot_dw_ss_it_BG       * nstk_now *tilleragefac

    !--- Total internodes sink strengths
    tot_gresp_it    = tot_gresp_it_AG + tot_gresp_it_BG
    tot_mresp_it    = tot_mresp_it_AG + tot_mresp_it_BG
    tot_dw_ss_it    = tot_dw_ss_it_AG + tot_dw_ss_it_BG

    !--------------------------!
    !--- Crop Sink Strength ---!
    !--------------------------!

    !--- Total substrates needed for crop growth [gCH2O m-2]
    dtcrss = dtitss + dtlfss + dtrtss

    !--- Total substrates needed for crop growth/maintenance respiration [gCH2O m-2]
    tot_gresp_crop = (tot_gresp_it_AG + tot_gresp_it_BG) + &
    & tot_gresp_lf + tot_gresp_rt
    tot_mresp_crop = (tot_mresp_it_AG + tot_mresp_it_BG) + &
    & tot_mresp_lf + tot_mresp_rt
    tot_dw_ss_crop = (tot_dw_ss_it_AG + tot_dw_ss_it_BG) + &
    & tot_dw_ss_lf + tot_dw_ss_rt

    !------------------------------!
    !--- Relative Sink Strength ---!
    !------------------------------!

    !--- Daily Relative Sink Strength by organ pool
    if(dtcrss .gt. 0.000000d0)then !To avoid NaN
        dr_rtss =   dtrtss / dtcrss
        dr_lfss =   dtlfss / dtcrss
        dr_itss =   dtitss / dtcrss
    else
        dr_rtss =   0.d0
        dr_lfss =   0.d0
        dr_itss =   0.d0
    endif

    !--- Carbon Balance Check
    if(dtcrss .gt. 0.000000d0)then
        if((dr_rtss + dr_lfss + dr_itss) .lt. 1.d0)then
            !--- Send to roots
            dr_rtss = dr_rtss + 1.d0 - (dr_rtss + dr_lfss + dr_itss)
        else if((dr_rtss + dr_lfss + dr_itss) .gt. 1.d0)then
            !--- Reduce roots pf
            dr_rtss = dr_rtss - ((dr_rtss + dr_lfss + dr_itss) - &
            & 1.d0)
        endif
    endif

    !--- Below ground fraction of growing internodes (considering leaf profile as the growing phytomers)
    if(dtitss_AG .gt. 0.d0) frac_AG = max(0.d0, min(1.d0, &
    & dtitss_AG/dtitss))
    if(dtitss_BG .gt. 0.d0) frac_BG = max(0.d0, min(1.d0, &
    & dtitss_BG/dtitss))

    !-------------------!
    !--- CROP GROWTH ---!
    !-------------------!

    if(.not. flemerged)then

        !----------------------------------------------------------------!
        !--- Available CH2O for Growth & Maintenance Before Emergence ---!
        !----------------------------------------------------------------!

        !--- Available substrates for crop growth and maintenance [tonCH2O ha-1]
        subsres             = sug_it_BG             ! Use only below ground sugar pool      [ton ha-1]

        !--- CO2 Assimilation rate
        dtg                 = 0.d0                  ! No CO2 assimilation before emergence  [ton ha-1]

    else

        !----------------------------------------------------------------!
        !--- Available CH2O for Growth & Maintenance After Emergence ---!
        !----------------------------------------------------------------!

        !--- Available substrates for crop growth and maintenance [tonCH2O ha-1]
        subsres             = sug_it_BG    ! Use only below ground sugar pool      [ton ha-1]

        !----------------------!
        !--- Photosynthesis ---!
        !----------------------!

        !--- Select among photosynthesis methods
        select case(metpg)

        case(1)

            !------------------!
            !--- RUE method ---!
            !------------------!

            !--- WARNING: This method do not take into account Growth + Maintenance Respiration. The reason is because most (all to date) of RUE values found in literature
            !--- are computed based on dry biomass ~ IPAR. In other words, when dry biomass is weighted part of maintenance respiration is lost in the transition time of sampling-weighting
            !--- AND the most important: the growth respiration has already gone, otherwise we couldnt be weighting anything!

            !--- Reduced Radiation Use Efficiency [gDW/MJPAR/m2]
            rue_mod = rue * agefactor_rue * tempfac_pho * &
            & pho_fac_co2 * swfacp

            !--- Carbon Gain [gDW/m2]
            dtg     = rue_mod * li * par_rad

            !--- Convert to ton ha-1
            dtg     =   dtg * (1.e4/1.e6)

        case(2)

            !----------------------------------------!
            !--- CO2 Assimilation by Canopy Layer ---!
            !----------------------------------------!

            !--- Astrological calculations for difuse and direct PAR interception
            call astro_sam(logf,swscre,doy,lat,dayl,daylp, &
            & sinld,cosld,dsinb,dsinbe_sam,dso)

            !--- Convert CO2 Assimilation rate to kgCO2 ha-1 h-1
            amax_conv   = amax / 1.e3 * 1.e4 * 3600 * 44.d0 / 1.e6

            !--- Convert Quantum Efficiency to kgCO2 ha-1 h-1 (J m-2 s-1)-1
            eff_conv    = eff / 1.e3 * 1.e4 * 3600 * 44.d0 / 1.e6 * 4.6

            !--- Reduced Maximum Assimilation Rate [mmol m-2 s-1]
            !--- Here we assume temperature, water stress and feedback response mainly affect the maximum assimilation rate, rather than quantum eff.
            amax_mod = amax_conv * tempfac_pho * swfacp * &
            & amaxfbfac * pho_fac_co2
            eff_mod  = eff_conv

            !--- LAI for assimilation
            lai_ass  = lai

            !--- Total assimilation for three canopy layers on hourly-step (Gaussian Integration) - Groudriaan
            call totass_samuca(doy, &
            & dayl, &
            & amax_mod, &
            & eff_mod, &
            & lai_ass, &
            & kdif, &
            & c_scattering, &
            & srad * 1.e6,  &        ! Solar Radiation [J/m2/day] - PAR [W/m2] is computed within radiat()
            & sinld, &
            & cosld, &
            & dtg, &                 ! Output
            & Acanopy, &             ! Output
            & Qleaf, &               ! Output
            & incpar, &              ! Output
            & photo_layer_act, &     ! Output
            & frac_li)             ! Output

            !--- Convert CO2 assimilation to CH2O assimilation rate [kgCH2O ha-1] (stoichiometric conversion)
            dtg             = dtg               * 30.d0/44.d0
            photo_layer_act = photo_layer_act   * 30.d0/44.d0

            !--- Convert to ton ha-1
            dtg             = dtg               * 1.e-3
            photo_layer_act = photo_layer_act   * 1.e-3

        case(3)

            !-----------------------------------!
            !--- Canopy Gross Photosynthesis ---!
            !-----------------------------------!

            !--- LAI for assimilation
            lai_ass  = lai

            !--- Canopy gross photosysntesis rate
            call PGS(swfacp,1.,1.,1.,chustk,par_rad,lai_ass,dtg,resp, &
            & diac,tmed,dw_total,CCEFF,CCMAX,k_can,PHTMAX,CCMP,PARMAX)

            !--- Growth and Maintenance respiration (computed on PGS subroutine)
            dtg = max(0.d0,dtg)

        end  select
    endif

    !----------------------!
    !--- Carbon Balance ---!
    !----------------------!

    !--- Available CH2O for leaves, internodes and roots [g m-2]
    dtg_avail_rt        = dtg       * dr_rtss * (1.e6/1.e4)
    dtg_avail_lf        = dtg       * dr_lfss * (1.e6/1.e4)
    dtg_avail_it        = dtg       * dr_itss * (1.e6/1.e4)
    dtg_avail_it_BG     = dtg       * dr_itss * (1.e6/1.e4) * frac_BG
    dtg_avail_it_AG     = dtg       * dr_itss * (1.e6/1.e4) * frac_AG

    !--- Available CH2O reserves in case its needed [g m-2]
    subsres_avail_rt    = subsres   * dr_rtss * (1.e6/1.e4)
    subsres_avail_lf    = subsres   * dr_lfss * (1.e6/1.e4)
    subsres_avail_it    = subsres   * dr_itss * (1.e6/1.e4)
    subsres_avail_it_BG = subsres   * dr_itss * (1.e6/1.e4) * frac_BG
    subsres_avail_it_AG = subsres   * dr_itss * (1.e6/1.e4) * frac_AG

    !---  Crop will use its reserves to growth before emergence (searching for light!) [ton ha-1]
    if(fl_use_reserves)then

        !------------------------------!
        !--- Use reserves to growth ---!
        !------------------------------!

        !--- Total substrates available to growth [g m-2]
        subs_avail_growth_crop  = (dtg + subsres) * (1.e6/1.e4)

        !--- This condition will happen before emergence [g m-2]
        subs_avail_growth_rt    = dtg_avail_rt  	+subsres_avail_rt
        subs_avail_growth_lf    = dtg_avail_lf    +subsres_avail_lf
        subs_avail_growth_it    = dtg_avail_it    +subsres_avail_it
        subs_avail_growth_it_BG = dtg_avail_it_BG +subsres_avail_it_BG
        subs_avail_growth_it_AG = dtg_avail_it_AG +subsres_avail_it_AG

        !--- Carbon balance
        subsres_avail_rt    = 0.d0
        subsres_avail_lf    = 0.d0
        subsres_avail_it    = 0.d0
        subsres_avail_it_BG = 0.d0
        subsres_avail_it_AG = 0.d0

    else

        !-------------------------------------------!
        !--- Use only CO2 assimilation to growth ---!
        !-------------------------------------------!

        !--- Total substrates available to growth [g m-2]
        subs_avail_growth_crop  = dtg * (1.e6/1.e4)

        !--- Still the reserves will be used for maintenance when renecessary [g m-2]
        subs_avail_growth_rt    = dtg_avail_rt
        subs_avail_growth_lf    = dtg_avail_lf
        subs_avail_growth_it    = dtg_avail_it
        subs_avail_growth_it_BG = dtg_avail_it_BG
        subs_avail_growth_it_AG = dtg_avail_it_AG

    endif

    !------------------------------!
    !--- Crop Source-Sink Ratio ---!
    !------------------------------!

    if(dtcrss .gt. 0.d0) cr_source_sink_ratio = subs_avail_growth_crop &
    &  / dtcrss

    !------------!
    !--- Leaf ---!
    !------------!
    call subs_balance(  subs_avail_growth_lf, &    ! Input
    & subsres_avail_lf, &        ! Input
    & dtlfss,           &        ! Input
    & tot_mresp_lf,     &         ! Input
    & tot_gresp_lf,     &        ! Input
    & tot_dw_ss_lf,     &        ! Input
    & sup_ratio_lf,     &        ! Output
    & supply_rate_lf,   &        ! Output
    & supply_used_lf,   &        ! Output
    & supply_used_mresp_lf, &    ! Output
    & supply_used_gresp_lf, &    ! Output
    & supply_used_dw_lf,    &    ! Output
    & reserves_used_mresp_lf, &  ! Output
    & maintenance_factor_lf,  &  ! Output
    & reduc_growth_factor_lf)  ! Output

    !------------!
    !--- Stem ---!
    !------------!
    call subs_balance(  subs_avail_growth_it,  &   ! Input
    & subsres_avail_it,      &     ! Input
    & dtitss,                &    ! Input
    & tot_mresp_it,          &   ! Input
    & tot_gresp_it,          &   ! Input
    & tot_dw_ss_it,          &   ! Input
    & sup_ratio_it,          &   ! Output
    & supply_rate_it,        &   ! Output
    & supply_used_it,        &   ! Output
    & supply_used_mresp_it,  &   ! Output
    & supply_used_gresp_it,  &   ! Output
    & supply_used_dw_it,     &   ! Output
    & reserves_used_mresp_it, &  ! Output
    & maintenance_factor_it, &   ! Output
    & reduc_growth_factor_it)  ! Output

    !-------------------------------!
    !--- Below ground Internodes ---!
    !-------------------------------!
    call subs_balance(subs_avail_growth_it_BG, &       ! Input
    & subsres_avail_it_BG,        &  ! Input
    & dtitss_BG,                  & ! Input
    & tot_mresp_it_BG,            &  ! Input
    & tot_gresp_it_BG,            &  ! Input
    & tot_dw_ss_it_BG,            &  ! Input
    & sup_ratio_it_BG,            &  ! Output
    & supply_rate_it_BG,          &  ! Output
    & supply_used_it_BG,          &  ! Output
    & supply_used_mresp_it_BG,    &  ! Output
    & supply_used_gresp_it_BG,    &  ! Output
    & supply_used_dw_it_BG,       &  ! Output
    & reserves_used_mresp_it_BG,  &  ! Output
    & maintenance_factor_it_BG,   &  ! Output
    & reduc_growth_factor_it_BG)   ! Output

    !-------------------------------!
    !--- Above ground Internodes ---!
    !-------------------------------!
    call subs_balance(  subs_avail_growth_it_AG,  &    ! Input
    & subsres_avail_it_AG,        &  ! Input
    & dtitss_AG,                  &  ! Input
    & tot_mresp_it_AG,            &  ! Input
    & tot_gresp_it_AG,            &  ! Input
    & tot_dw_ss_it_AG,            &  ! Input
    & sup_ratio_it_AG,            &  ! Output
    & supply_rate_it_AG,          &  ! Output
    & supply_used_it_AG,          &  ! Output
    & supply_used_mresp_it_AG,    &  ! Output
    & supply_used_gresp_it_AG,    &  ! Output
    & supply_used_dw_it_AG,       &  ! Output
    & reserves_used_mresp_it_AG,  &  ! Output
    & maintenance_factor_it_AG,   &  ! Output
    & reduc_growth_factor_it_AG)   ! Output

    !-------------!
    !--- Roots ---!
    !-------------!
    call subs_balance(  subs_avail_growth_rt, &    ! Input
    & subsres_avail_rt,       &  ! Input
    & dtrtss,                 &  ! Input
    & tot_mresp_rt,           &  ! Input
    & tot_gresp_rt,           &  ! Input
    & tot_dw_ss_rt,           &  ! Input
    & sup_ratio_rt,           &  ! Output
    & supply_rate_rt,         &  ! Output
    & supply_used_rt,         &  ! Output
    & supply_used_mresp_rt,   &  ! Output
    & supply_used_gresp_rt,   &  ! Output
    & supply_used_dw_rt,      &  ! Output
    & reserves_used_mresp_rt, &  ! Output
    & maintenance_factor_rt,  &  ! Output
    & reduc_growth_factor_rt)  ! Output

    !--- Overall Crop Carbon Balance
    supply_used_crop  = supply_used_rt + supply_used_it +  &
    & supply_used_lf
    
    supply_used_mresp_crop	     = supply_used_mresp_rt	   	+ &
    & supply_used_mresp_it   	+	 supply_used_mresp_lf
    
    supply_used_gresp_crop	     = supply_used_gresp_rt	   	+ &
    & supply_used_gresp_it   	+	 supply_used_gresp_lf
    
    supply_used_dw_crop	         = supply_used_dw_rt	   	+ &
    & supply_used_dw_it	   	+	 supply_used_dw_lf
    
    reserves_used_mresp_crop	 = reserves_used_mresp_rt  	+ &
    & reserves_used_mresp_it 	+	 reserves_used_mresp_lf
    
    maintenance_factor_crop	     = (maintenance_factor_rt  * &
    & dw_rt 		+	maintenance_factor_it  * dw_it  + &
    & maintenance_factor_lf  * dw_lf) / dw_total
    
    reduc_growth_factor_crop	 = (reduc_growth_factor_rt * &
    & dw_rt  	+	reduc_growth_factor_it * dw_it  + &
    & reduc_growth_factor_lf * dw_lf) / dw_total

    !--- Downscale to organ level to find structural and sugar partitioning factors
    dstr_it_BG  = 0.d0
    dsug_it_BG  = 0.d0
    dstr_it_AG  = 0.d0
    dsug_it_AG  = 0.d0

    !--- Total substrates available for leaves and internodes at the "reference stalk" level
    !--- Note that at below ground conditions tilleragefac should be 1.d0 (keep it here for debugging purpose)
    subs_avail_growth_lf_ref_till       = subs_avail_growth_lf &
    &   / (nstk_now  * tilleragefac)
    subs_avail_growth_it_ref_till       = subs_avail_growth_it &
    &   / (nstk_now  * tilleragefac)
    subs_avail_growth_it_BG_ref_till    = subs_avail_growth_it_BG &
    &   / (nstk_now  * tilleragefac)
    subs_avail_growth_it_AG_ref_till    = subs_avail_growth_it_AG &
    &   / (nstk_now  * tilleragefac)

    !--- Total substrates reserves in case needed for leaves and internodes at the "reference stalk" level
    !--- Note that all reserves are available for growth and respiration in (avail_subs_crop)
    subsres_avail_lf_ref_till           = subsres_avail_lf &
    &    / (nstk_now  * tilleragefac)
    subsres_avail_it_ref_till           = subsres_avail_it &
    &    / (nstk_now  * tilleragefac)
    subsres_avail_it_BG_ref_till        = subsres_avail_it_BG &
    &    / (nstk_now  * tilleragefac)
    subsres_avail_it_AG_ref_till        = subsres_avail_it_AG &
    &    / (nstk_now  * tilleragefac)

    !--- Total sink strength of all leaves and internodes
    dtlfss_ref_till             = dtlfss        / (nstk_now  * &
    & tilleragefac)
    dtitss_ref_till             = dtitss        / (nstk_now  * &
    & tilleragefac)
    dtitss_BG_ref_till          = dtitss_BG     / (nstk_now  * &
    & tilleragefac)
    dtitss_AG_ref_till          = dtitss_AG     / (nstk_now  * &
    & tilleragefac)

    !----------------------!
    !--- Phytomer Level ---!
    !----------------------!
    do phy = 1, n_ph

        !--- Check which temperature to use (soil or air) based on phytomer position
        if(fl_it_AG(phy))then
            !--- Above ground
            diphy   = di
            t_mresp = tmed
        else
            !--- Below ground
            diphy   = disoil
            t_mresp = soiltemperature
        endif

        !----------------------!
        !--- Phytomer Flags ---!
        !----------------------!

        !--- Has an active leaf
        if(phy .le. n_lf_alive)then
            fl_hasleaf = .true.
        else
            fl_hasleaf = .false.
        endif

        !--- Has a growing internode
        if(phy .gt. n_lf_it_form)then
            fl_it_visible   = .true.
        else
            fl_it_visible   = .false.
        endif

        if(fl_hasleaf)then

            !------------!
            !--- Leaf ---!
            !------------!

            !--- Initial sink strengths of leaf
            dtlfss_phy      = phprof(phy,2)
            mresp_lf_phy    = phprof(phy,25)
            gresp_lf_phy    = phprof(phy,26)
            dw_ss_lf_phy    = phprof(phy,27)

            !--- Relative Sink Strength
            if(dtlfss_ref_till .gt. 0.d0) rel_ss_lf_phy    = dtlfss_phy &
            &  / dtlfss_ref_till

            !--- Substrates available for this leaf
            subs_avail_growth_lf_phy    = subs_avail_growth_lf_ref_till &
            &  * rel_ss_lf_phy

            !--- Reserves available for this leaf in case needed
            subsres_avail_lf_phy        = subsres_avail_lf_ref_till * &
            &  rel_ss_lf_phy

            !--- Leaf Growth
            call subs_balance(  subs_avail_growth_lf_phy, &   ! Input
            & subsres_avail_lf_phy,       &  ! Input
            & dtlfss_phy,                 &  ! Input
            & mresp_lf_phy,               &  ! Input
            & gresp_lf_phy,               &  ! Input
            & dw_ss_lf_phy,               &  ! Input
            & sup_ratio_lf_phy,           &  ! Output
            & supply_rate_lf_phy,         &  ! Output
            & supply_used_lf_phy,         &  ! Output
            & supply_used_mresp_lf_phy,   &  ! Output
            & supply_used_gresp_lf_phy,   &  ! Output
            & supply_used_dw_lf_phy,      &  ! Output
            & reserves_used_mresp_lf_phy, &  ! Output
            & maintenance_factor_lf_phy,  &  ! Output
            & reduc_growth_factor_lf_phy)  ! Output

            !--- Update phytomer profile rates
            phprof(phy,28)  = sup_ratio_lf_phy
            phprof(phy,29)  = supply_rate_lf_phy
            phprof(phy,30)  = supply_used_lf_phy
            phprof(phy,31)  = supply_used_mresp_lf_phy
            phprof(phy,32)  = supply_used_gresp_lf_phy
            phprof(phy,33)  = supply_used_dw_lf_phy
            phprof(phy,34)  = maintenance_factor_lf_phy
            phprof(phy,35)  = reduc_growth_factor_lf_phy

            !--- Leaf Blade Area Gain (cm2)
            if(flemerged)then
                if(fl_lf_AG(phy))then
                    dla_phy             = supply_used_dw_lf_phy  * sla_sam
                    phprof(phy, 4)      = dla_phy
                    dla_gain_ref_till   = dla_gain_ref_till +  dla_phy
                endif
            endif
        endif

        !-----------------!
        !--- Internode ---!
        !-----------------!

        !--- Initial sink strengths of internode [gCH2O]
        dtitss_phy      = phprof(phy,7)
        mresp_it_phy    = phprof(phy,36)
        gresp_it_phy    = phprof(phy,37)
        dw_ss_it_phy    = phprof(phy,38)

        if(fl_it_AG(phy))then

            !--- Above Ground

            !--- Relative Sink Strength [0-1]
            if(dtitss_AG_ref_till .gt. 0.d0) rel_ss_it_phy    = &
            & dtitss_phy / dtitss_AG_ref_till

            !--- Substrates available for internode growth [g]
            subs_avail_growth_it_phy = &
            & subs_avail_growth_it_AG_ref_till * rel_ss_it_phy

            !--- Reserves available for this leaf in case needed
            subsres_avail_it_phy    = &
            & subsres_avail_it_AG_ref_till * rel_ss_it_phy

        else

            !--- Below Ground

            !--- Relative Sink Strength [0-1]
            if(dtitss_BG_ref_till .gt. 0.d0) rel_ss_it_phy    = &
            & dtitss_phy / dtitss_BG_ref_till

            !--- Substrates available for internode growth [g]
            subs_avail_growth_it_phy = &
            & subs_avail_growth_it_BG_ref_till * rel_ss_it_phy

            !--- Reserves available for this leaf in case needed
            subsres_avail_it_phy    = &
            & subsres_avail_it_BG_ref_till * rel_ss_it_phy

        endif

        !--- Internode Growth [g]
        call subs_balance(  subs_avail_growth_it_phy,  &   ! Input
        & subsres_avail_it_phy,       &  ! Input
        & dtitss_phy,                 &  ! Input
        & mresp_it_phy,               &  ! Input
        & gresp_it_phy,               &  ! Input
        & dw_ss_it_phy,               &  ! Input
        & sup_ratio_it_phy,           &  ! Output
        & supply_rate_it_phy,         &  ! Output
        & supply_used_it_phy,         &  ! Output
        & supply_used_mresp_it_phy,   &  ! Output
        & supply_used_gresp_it_phy,   &  ! Output
        & supply_used_dw_it_phy,      &  ! Output
        & reserves_used_mresp_it_phy, &  ! Output
        & maintenance_factor_it_phy,  &  ! Output
        & reduc_growth_factor_it_phy)  ! Output

        !--- Internode Age [Cdays]
        age_it_phy      = phprof(phy,58)

        !--- Structural partitioning factor [0-1]
        it_struc_pfac_rate = it_struc_pfac( it_struc_tb_ini, &
        & it_struc_to1,  &
        & it_struc_to2,  &
        & it_struc_tb_end, &
        & it_struc_pfac_temp_max_red, &
        & it_struc_pfac_wate_max_red, &
        & it_struc_pfac_max, &
        & it_struc_pfac_min, &
        & it_struc_pfac_tb, &
        & it_struc_pfac_tm, &
        & it_struc_pfac_te, &
        & it_struc_pfac_delta, &
        & age_it_phy, &
        & t_mresp, &
        & swfacf)                     ! From SOPLAT

        !--- Total Sugars and Structural rate [g]
        dstr_it_phy  = it_struc_pfac_rate           * &
        & supply_used_dw_it_phy
        dsug_it_phy  = (1.d0 - it_struc_pfac_rate)  * &
        & supply_used_dw_it_phy

        !--- Update phytomer profile
        phprof(phy,39)  = sup_ratio_it_phy
        phprof(phy,40)  = supply_rate_it_phy
        phprof(phy,41)  = supply_used_it_phy
        phprof(phy,42)  = supply_used_mresp_it_phy
        phprof(phy,43)  = supply_used_gresp_it_phy
        phprof(phy,44)  = supply_used_dw_it_phy
        phprof(phy,45)  = maintenance_factor_it_phy
        phprof(phy,46)  = reduc_growth_factor_it_phy
        phprof(phy,47)  = supply_used_dw_it_phy
        phprof(phy,48)  = dstr_it_phy
        phprof(phy,49)  = dsug_it_phy
        phprof(phy,15)  = it_struc_pfac_rate

        !--- Integrate structural and sugar parts rates
        if(fl_it_AG(phy))then

            !--- Above Ground
            dstr_it_AG  = dstr_it_AG    +   dstr_it_phy
            dsug_it_AG  = dsug_it_AG    +   dsug_it_phy

        else

            !--- Below Ground
            dstr_it_BG  = dstr_it_BG    +   dstr_it_phy
            dsug_it_BG  = dsug_it_BG    +   dsug_it_phy

        endif
    enddo

    !--- Upscale Structural and Sugar Rates to Field Level [g m-2]
    dstr_it_BG  = dstr_it_BG	* nstk_now * tilleragefac
    dsug_it_BG  = dsug_it_BG	* nstk_now * tilleragefac
    dstr_it_AG  = dstr_it_AG	* nstk_now * tilleragefac
    dsug_it_AG  = dsug_it_AG	* nstk_now * tilleragefac

    !--- Correction factor to meet carbon balance
    dsug_corr_fac_BG   = 1.d0
    dsug_corr_fac_AG   = 1.d0
    if((dstr_it_BG + dsug_it_BG) .gt. 0.000000d0 .and. &
    & supply_used_dw_it_BG .gt. 0.d0) dsug_corr_fac_BG   = &
    & supply_used_dw_it_BG / (dstr_it_BG + dsug_it_BG)
    if((dstr_it_AG + dsug_it_AG) .gt. 0.000000d0 .and. &
    & supply_used_dw_it_AG .gt. 0.d0) dsug_corr_fac_AG   = &
    & supply_used_dw_it_AG / (dstr_it_AG + dsug_it_AG)

    !--- Check carbon balance discrepancy
    c_check_tol = 0.10
    if(abs(1.d0 - dsug_corr_fac_BG) .gt. c_check_tol)then
        if(flprompt_msg)then
            write(warn,*) 'More than 10% Carbon Balance Discrepancy'// &
            & ' on Internode Below Ground Sugar/Structural Partitioning'// &
            & ' at DAS: ',das, &
            & 'Discrepancy of ', abs(1.d0 - dsug_corr_fac_BG) * 100.d0, &
            &  '% was corrected.'
        endif
    endif

    c_check_tol = 0.10
    if(abs(1.d0 - dsug_corr_fac_AG) .gt. c_check_tol)then
        if(flprompt_msg)then
            write(warn,*) 'More than 10% Carbon Balance Discrepancy'// &
            & ' on Internode Above Ground Sugar/Structural Partitioning'// &
            & ' at DAS: ',das, &
            & 'Discrepancy of ', abs(1.d0 - dsug_corr_fac_AG) * 100.d0, &
            &  '% was corrected.'
        endif
    endif

    !--- Correct to meet carbon balance
    dstr_it_BG  = dstr_it_BG  *   dsug_corr_fac_BG
    dsug_it_BG  = dsug_it_BG	*   dsug_corr_fac_BG

    !--- Biomass Rates [ton ha-1]
    ddw_rt      = supply_used_dw_rt     * (1.e4/1.e6)
    ddw_lf      = supply_used_dw_lf     * (1.e4/1.e6)
    ddw_it      = supply_used_dw_it     * (1.e4/1.e6)
    ddw_it_BG   = supply_used_dw_it_BG  * (1.e4/1.e6)
    ddw_it_AG   = supply_used_dw_it_AG  * (1.e4/1.e6)
    dstr_it_BG  = dstr_it_BG            * (1.e4/1.e6)
    dsug_it_BG  = dsug_it_BG	        * (1.e4/1.e6)
    dstr_it_AG  = dstr_it_AG	        * (1.e4/1.e6)
    dsug_it_AG  = dsug_it_AG	        * (1.e4/1.e6)

    !--- Leaf Area Index Gain [m2 m-2]
    dlai_gain   = dla_gain_ref_till     * &
    & (nstk_now * tilleragefac) / 1.e4

    !--- Exceeded Substrates [tonCH2O ha-1]
    if(dtg_avail_lf .gt. dtlfss) exc_dtg_lf = &
    & (dtg_avail_lf - dtlfss) * (1.e4/1.e6)
    if(dtg_avail_it .gt. dtitss) exc_dtg_it = &
    & (dtg_avail_it - dtitss) * (1.e4/1.e6)
    if(dtg_avail_rt .gt. dtrtss) exc_dtg_rt = &
    & (dtg_avail_rt - dtrtss) * (1.e4/1.e6)

    !--- Net Rate of Reserves
    if(fl_use_reserves)then

        if(.not. dtg_avail_lf .gt. dtlfss) &
        & reserves_used_growth_lf = supply_used_lf - dtg_avail_lf
        if(.not. dtg_avail_it .gt. dtitss) &
        & reserves_used_growth_it = supply_used_it - dtg_avail_it
        if(.not. dtg_avail_rt .gt. dtrtss) &
        & reserves_used_growth_rt = supply_used_rt - dtg_avail_rt

    else
        reserves_used_growth_lf = 0.d0
        reserves_used_growth_it = 0.d0
        reserves_used_growth_rt = 0.d0
    endif

    !--- Substrate reserves balance
    dsubsres_lf     = exc_dtg_lf - reserves_used_mresp_lf * &
    & (1.e4/1.e6) - reserves_used_growth_lf * (1.e4/1.e6)
    
    dsubsres_it     = exc_dtg_it - reserves_used_mresp_it * &
    & (1.e4/1.e6) - reserves_used_growth_it * (1.e4/1.e6)
    
    dsubsres_rt     = exc_dtg_rt - reserves_used_mresp_rt * &
    & (1.e4/1.e6) - reserves_used_growth_rt * (1.e4/1.e6)
    
    dsubsres        = dsubsres_lf + dsubsres_it + dsubsres_rt

    !--- Ratio Reserves Variation
    dsubsres_ratio  = 1.d0
    if(subsres .gt. 0.d0) dsubsres_ratio  = (subsres + dsubsres) &
    & / subsres

    !-------------------!
    !--- Stalk Rates ---!
    !-------------------!
    if(flemerged)then

        !--- Above ground conditions
        if(fl_stalk_emerged)then

            !-------------------------------------------!
            !--- Stalk has emerged from soil surface ---!
            !-------------------------------------------!

            !--- Plant extension rate
            do hour = 1, 24
                !--- Hourly plant extension rate [mm h-1]
                per_hour = dpercoeff * tempfac_h_per(hour) * swface * &
                & agefactor_per

                !--- Integrate to daily rate [mm day-1]
                per = per + per_hour
            enddo

            !--- Fraction of stalk extension partitioned among internodes as function of structural gain
            dtot_str_dw_ref_till = sum(phprof(1:n_ph,15))

            if(dtot_str_dw_ref_till .gt. 0.d0)then
                !--- Only extends when structural gain is positive
                do phy = 1, n_ph
                    !--- Internode extension rate [mm]
                    per_it_phy     = min(max_per_it, per * &
                    & (phprof(phy,15) / dtot_str_dw_ref_till)) ! dLength [mm] &
                    phprof(phy,23) = per_it_phy ! dLength [mm]
                enddo
            endif

            !--------------------------------!
            !--- Water Fraction in Stalks ---!
            !--------------------------------!

            !--- Computing water fraction rate of Stalks for Stalk Fresh Mass [Fresh Cane Yield: ton ha-1]
            !--- Following Martines SASTA 2001 and Mathew Jones (CANEGRO)
            dwat_it_AG = (dswat_ddws * ddw_it_AG) - &
            & (dswat_dsuc * dsug_it_AG)

        endif

    else

        !--- Crop is still below the ground surface
        !--- Shoot extension towards soil surface [cm]
        dshootext_BG    =   dshootext_BG_rate     *   disoil

    endif

    !-------------------!
    !--- Root Growth ---!
    !-------------------!

    !--- Root Front velocity [cm day-1]
    drdepth = rootdrate * disoil

    !--- Number of sublayers for root profile rate
    nsublay = aint(dep(numlay))

    !--- calculate RLD rate
    if (cumdenstb .eq. 1) then    
        
        !--- if cumdenstb .eq. 1 calculate by simple root factor [e.g. cumdens]    
        !--- Assume specific root length density as srlmin
        srl  = srlmin

        !--- get drld profile
        call root_growth_factor(cumdenstb, ddw_rt,                      &
                                      upper, bottom, SLTHICKNESS, dep,  & 
                                      rootshape, drld)
    else
    
        !--- Root Profile Rate (RLD and Biomass)
        call root_profile(numlay,   & ! Input
        & nsublay,                  &  ! Input
        & initcropdepth,            &  ! Input
        & rpup,                     &  ! Input
        & effective_rd,             &  ! Input
        & root_front_size,          &  ! Input
        & srlmax,                   &  ! Input
        & srlmin,                   &  ! Input
        & bottom,                   &  ! Input
        & slthickness,              &  ! Input
        & ddw_rt,                   &  ! Input
        & flemerged,                &  ! Input
        & drld,                     &  ! Output
        & ddw_rt_sl)                 ! Output
    
    endif

    !--- Root Senescence (PIT)
    ddw_rt_dead = 0.d0
    drld_dead   = 0.d0

    !------------------------!
    !--- Step Integration ---!
    !------------------------!

    !--- Phytomer Level Integration
    do phy   = 1, n_ph

        !--- Integrate phytomer attributes
        phprof(phy, 1)  = phprof(phy, 1) + phprof(phy,55) ! Leaf Age                    [Cdays]
        phprof(phy,58)  = phprof(phy,58) + phprof(phy,57) ! Internode Age               [Cdays]
        phprof(phy,12)  = phprof(phy,12) + phprof(phy,56) ! Phytomer Age                [Cdays]
        phprof(phy, 6)  = phprof(phy, 6) + phprof(phy,33) ! Leaf Dry Weight             [g]
        phprof(phy, 5)  = phprof(phy, 5) + phprof(phy, 4) ! Leaf Area                   [cm2]
        phprof(phy,16)  = phprof(phy,16) + phprof(phy,23) ! Internode length            [mm]

        if(.not. fl_it_AG(phy))then

            !--- Carbon balance of growth
            dsug_it_phy_growth      = phprof(phy,49)
            dstr_it_phy_growth      = phprof(phy,48)
            ddw_it_phy_growth       = phprof(phy,47)

            !--- Carbon balance of reserves
            dsug_it_phy_reserves    = max(0.d0, (phprof(phy,52)  * &
            &   dsubsres_ratio) - phprof(phy,52))
            ddw_it_phy_reserves     = dsug_it_phy_reserves

            !--- Current State
            dw_it_phy               = phprof(phy,50)
            str_it_phy              = phprof(phy,51)
            sug_it_phy              = phprof(phy,52)

            !--- Integrate Total Dry Weight and Sugars [g]
            dw_it_phy      = dw_it_phy  + ddw_it_phy_growth     + &
            &   ddw_it_phy_reserves
            str_it_phy     = str_it_phy + dstr_it_phy_growth
            sug_it_phy     = sug_it_phy + dsug_it_phy_growth    + &
            &   dsug_it_phy_reserves

            !--- Update internode profile
            phprof(phy,50)  = dw_it_phy  ! Internode Dry Weight        [g]
            phprof(phy,51)  = str_it_phy ! Internode Structural Weight [g]
            phprof(phy,52)  = sug_it_phy ! Internode Sugars Weight     [g]

            !--- Sucrose and Hexose Contents at below ground internodes [g]
            !--- 50% shared due to unkown
            suc_it_phy  =   frac_suc_BG   *   sug_it_phy
            hex_it_phy  =   frac_hex_BG   *   sug_it_phy

            !--- Below Ground
            suc_it_BG_ref_till  = suc_it_BG_ref_till    + &
            &  suc_it_phy
            hex_it_BG_ref_till  = hex_it_BG_ref_till    + &
            &  hex_it_phy

        else

            !--- Carbon balance of growth
            dsug_it_phy_growth      = phprof(phy,49)
            dstr_it_phy_growth      = phprof(phy,48)
            ddw_it_phy_growth       = phprof(phy,47)

            !--- Current State
            dw_it_phy               = phprof(phy,50)
            str_it_phy              = phprof(phy,51)
            sug_it_phy              = phprof(phy,52)

            !--- Integrate Total Dry Weight and Sugars [g]
            dw_it_phy   = dw_it_phy  + ddw_it_phy_growth    ! Internode Dry Weight        [g]
            str_it_phy  = str_it_phy + dstr_it_phy_growth   ! Internode Structural Weight [g]
            sug_it_phy  = sug_it_phy + dsug_it_phy_growth   ! Internode Sugars Weight     [g]

            !--- Update internode profile
            phprof(phy,50)  = dw_it_phy  ! Internode Dry Weight        [g]
            phprof(phy,51)  = str_it_phy ! Internode Structural Weight [g]
            phprof(phy,52)  = sug_it_phy ! Internode Sugars Weight     [g]

            !--- Sucrose and Hexose Contents at above ground internodes
            call sucrose_content(   dw_it_phy, &             ! Input Variable
            & sug_it_phy,            & ! Input Variable
            & suc_min,               & ! Input Parameter
            & hex_min,               & ! Input Parameter
            & suc_acc_ini,           & ! Input Parameter
            & suc_frac_rate_ts,      & ! Input Parameter
            & suc_it_phy,            & ! Output Variable
            & hex_it_phy)            ! Output Variable

            !--- Above Ground
            suc_it_AG_ref_till  = suc_it_AG_ref_till    + &
            &  suc_it_phy
            hex_it_AG_ref_till  = hex_it_AG_ref_till    + &
            &  hex_it_phy

        endif

        !--- Store in phytomer profile
        phprof(phy,53)  = suc_it_phy ! Internode sucrose weight [g]
        phprof(phy,54)  = hex_it_phy ! Internode hexoses weight [g]

        !--- Fractions of Fiber/Sugars/Sucrose/Hexose
        if(phprof(phy,50) .gt. 0.d0)then
            phprof(phy,17)  = phprof(phy,51) / phprof(phy,50)
            phprof(phy,18)  = phprof(phy,52) / phprof(phy,50)
            phprof(phy,19)  = phprof(phy,53) / phprof(phy,50)
            phprof(phy,20)  = phprof(phy,54) / phprof(phy,50)
        endif

    enddo

    !--- Field Level Biomass Integration [ton ha-1]
    dw_rt       =   dw_rt       +   ddw_rt      -   ddw_rt_dead
    dw_lf       =   dw_lf       +   ddw_lf      -   ddw_lf_dead &
    &  -   ddw_lf_shed     +   ddw_lf_appear
    dw_it       =   dw_it       +   ddw_it      -   ddw_it_dead &
    &  +   dsubsres
    dw_it_AG    =   dw_it_AG    +   ddw_it_AG   -   ddw_it_AG_dead
    str_it_AG   =   str_it_AG   +   dstr_it_AG  -   dstr_it_AG_dead
    sug_it_AG   =   sug_it_AG   +   dsug_it_AG  -   dsug_it_AG_dead
    dw_it_BG    =   dw_it_BG    +   ddw_it_BG   -   ddw_it_BG_dead &
    &  +   dsubsres
    str_it_BG   =   str_it_BG   +   dstr_it_BG  -   dstr_it_BG_dead
    sug_it_BG   =   sug_it_BG   +   dsug_it_BG  -   dsug_it_BG_dead &
    &  +   dsubsres
    wat_it_AG   =   wat_it_AG   +   dwat_it_AG  -   dwat_it_AG_dead

    !--- Upscale Sucrose/Hexose to Field Level [ton ha-1]
    suc_it_AG   =   suc_it_AG_ref_till * (nstk_now * tilleragefac) &
    &  * (1.e4 / 1.e6)
    hex_it_AG   =   hex_it_AG_ref_till * (nstk_now * tilleragefac) &
    &  * (1.e4 / 1.e6)
    suc_it_BG   =   suc_it_BG_ref_till * (nstk_now * tilleragefac) &
    &  * (1.e4 / 1.e6)
    hex_it_BG   =   hex_it_BG_ref_till * (nstk_now * tilleragefac) &
    &  * (1.e4 / 1.e6)

    !--- Correction factor to meet carbon balance
    dsug_corr_fac_BG   = 1.d0
    dsug_corr_fac_AG   = 1.d0
    if((suc_it_BG + hex_it_BG) .gt. 0.00001) dsug_corr_fac_BG   = &
    &  sug_it_BG / (suc_it_BG + hex_it_BG)
    if((suc_it_AG + hex_it_AG) .gt. 0.00001) dsug_corr_fac_AG   = &
    &  sug_it_AG / (suc_it_AG + hex_it_AG)

    !--- Check carbon balance discrepancy
    c_check_tol = 0.10d0
    if(abs(1.d0 - dsug_corr_fac_BG) .gt. c_check_tol)then
        if(flprompt_msg)then
            write(warn,*) 'More than 10% Carbon Balance Discrepancy'// &
            & ' on Internode Below Ground Sugar/Structural Partitioning'// &
            & ' at DAS: ',das, &
            & 'Discrepancy of ', abs(1.d0 - dsug_corr_fac_BG) * 100.d0, &
            &  '% was corrected.'
        endif
    endif

    c_check_tol = 0.10d0
    if(abs(1.d0 - dsug_corr_fac_AG) .gt. c_check_tol)then
        if(flprompt_msg)then
            write(warn,*) 'More than 10% Carbon Balance Discrepancy'// &
            & ' on Internode Above Ground Sugar/Structural Partitioning'// &
            & ' at DAS: ',das, &
            & 'Discrepancy of ', abs(1.d0 - dsug_corr_fac_AG) * 100.d0, &
            &  '% was corrected.'
        endif
    endif

    suc_it_AG   =   suc_it_AG	*	dsug_corr_fac_AG
    hex_it_AG   =   hex_it_AG	*	dsug_corr_fac_AG
    suc_it_BG   =   suc_it_BG	*	dsug_corr_fac_BG
    hex_it_BG   =   hex_it_BG	*	dsug_corr_fac_BG

    !--- Total Dry Biomass [ton ha-1]
    dw_total    =   dw_it       +   dw_lf   +   dw_rt

    !--- Stalk Fresh Mass [ton ha-1]
    fw_it_AG    =   dw_it_AG    +   max(0.d0, wat_it_AG) ! Avoid Negative Mass that can happen depending on parameters setup (dswat_dsuc & dswat_dstr)

    !--- Overall sugar content in dry mass basis
    if(dw_it_AG .gt. 0.d0) sug_cont    = sug_it_AG / dw_it_AG

    !--- Sucrose content on Fresh Stalk basis (POL%)
    if(sug_cont .gt. suc_acc_ini) then
        pol          = suc_it_AG / fw_it_AG * 100.d0
        wat_con      = 1.d0 - (dw_it_AG / fw_it_AG)
    else
        pol      = 0.d0 ! To Avoid high pol values at early growth
        wat_con  = 0.85
    endif

    !--- Aerial Dry Biomass [ton ha-1]
    if(flemerged)then
        !--- After emergence
        dw_aerial   =   dw_it_AG    +   dw_lf
    else
        !--- Leaf Dry Weight is at below ground (Shoot)
        dw_aerial   =   dw_it_AG
    endif

    !--- Plant Population [tillers m-2]
    nstk    =   nstk    +   dnstk

    !--- Absolute number of tillers
    atln = aint(max(0.d0, nstk))
    if(.not. (atln .eq. nstk))then
        !--- Increase one tiller to account for decimals
        atln    =   atln + 1
    endif

    if(flemerged) nstk_now = nstk ! nstk_now consider the below ground tillers while crop is not emrged
    if(flemerged) atln_now = atln ! atln_now consider the below ground tillers while crop is not emrged

    !--- Relative Age Difference Among Tillers [0-1]
    do tl = 1, atln
        tillerageprof(tl,1) = tillerageprof(tl,1) + disoil
        tillerageprof(tl,2) = (tillerageprof(tl,1) / &
        & tillerageprof(1,1)) ** tilleragefac_adjust
    enddo

    !--- Update Tiller Age Factor
    !--- This factor is necessary to avoid overpredictions when upscaling all tillers to field level
    !--- This factor could be avoided if all tillers were simulated to achieve Field-Plant-Organ Scales (next version).
    if(atln .lt. ini_nstk) then
        !--- Only primary tillers
        tilleragefac = 1.d0
    else
        !--- Secondary/tertiary/four... emerged!
        tilleragefac = max(0.01, sum(tillerageprof(1:atln,2)) / &
        & atln) ! Do not let tilleragefac <= Zero (avoid zero division)
    endif

    !--- Reallocate below ground sugars among new tillers
    if(abs(dnstk) .gt. 0.d0)then
        !--- Shared sugars [g] below ground sugars among below ground internodes
        shared_it_sug_BG =  (sug_it_BG * (1.e6/1.e4) / (nstk_now * &
        & tilleragefac)) / n_it_BG

        !--- First Below Ground Internode position
        pos_it_BG        = n_ph - n_it_BG + 1

        !--- Carbon balance including the new internode
        do phy = pos_it_BG, n_ph
            !--- Carbon balance of reserves
            ts_it_phy       =   shared_it_sug_BG
            dw_it_phy       =   phprof(phy,51)  +   ts_it_phy

            !--- Update below ground internodes DW and Sugars
            phprof(phy,50)  =   dw_it_phy                           ! All weight are sugars at initial step
            phprof(phy,52)  =   ts_it_phy                           ! Total sugars
            phprof(phy,53)  =   shared_it_sug_BG * frac_suc_BG    ! 50% share sucrose/hexose
            phprof(phy,54)  =   shared_it_sug_BG * frac_hex_BG    ! 50% share sucrose/hexose
        enddo
    endif

    !--- Stalk Height [m]
    stk_h   =   stk_h   +   per / 1.e3

    !--- Stalk diameter
    !--- Based on stalk population (Fabio Marin)
    if(nstk_now .lt. 9.d0) then
        diam_stk = -.077 * nstk_now + 3.0443
    else
        diam_stk = -.0256 * nstk_now**2. + .4206 *  nstk_now + .7763
    endif

    !--- Leaf Area Index [m2 m-2]
    lai         =   lai     +   dlai_gain   -   dlai_dead   - &
    & dlai_shed   +      dlai_gain_appear

    !--- Crop Coefficient (EORATIO)
    kc          = kc_min    + (eoratio - kc_min) * lai / (maxlai_eo)

    !--- Average Number of green leaves per tiller
    n_lf_tiller = n_lf_alive_AG * tilleragefac

    !--- Root Depth [cm]
    rd = min(rd + drdepth, rdm)

    !--- Effective Root Depth [cm]
    effective_rd = min(effective_rd + drdepth, rdm)
    if(.not. flemerged) then
        !--- Upward root growth
        rpup    = rpup + min(drdepth, dshootext_BG)
        rpup    = min(initcropdepth, rpup)
    endif

    !--- Root Length Density [cm.root cm-3.soil]
    rld      = rld  +   drld    -   drld_dead ! Arrays algebra

    !--- Phytomer appearance stimulus [0-1]
    phy_stimuli     = phy_stimuli + dphy_stimuli

    !--- Crop Age [Cdays]
    diac        =   diac        +   di
    diacsoil    =   diacsoil    +   disoil

    !--- Crop Age After Emergence [Cdays]
    if(flemerged) then
        diacem        = diacem        + di
        diacsoilem    = diacsoilem    + disoil
    endif

    !--- Memorize how much substrates is needed for emergence
    if(.not. flemerged)then
        !--- Learning how much substrates is needed
        res_used_emerg  =   res_used_emerg + supply_used_crop * &
        & (1.e4/1.e6)
    endif

    !--------------------!
    !--- Flags Update ---!
    !--------------------!

    if(cr_source_sink_ratio .lt. cr_source_sink_ratio_ruse .and. &
    & flemerged)then

    !--- Use reserves only for maintenance
    fl_use_reserves     = .false.

    else
        if(sug_it_BG .gt. (res_used_emerg * res_used_emerg_fac))then

            !--- Use reserves to growth and maintenance
            fl_use_reserves     = .true.
        else
            !--- Do not use reserves to growth if its not enough to the crop the emerge again (crop memory)
            fl_use_reserves     = .false.
        endif
    endif

    !--- Check phytomer appearence stimulus
    if(phy_stimuli .ge. 1.d0) then

        !-------------------------------!
        !--- Initiate a new phytomer ---!
        !-------------------------------!

        !--- Update Counters (leaf + internode + phytomers)
        n_ph                = n_ph              + 1
        n_it                = n_it              + 1
        n_lf                = n_lf              + 1
        n_lf_alive          = n_lf_alive        + 1
        n_lf_alive_juveni   = n_lf_alive_juveni + 1

        if(flemerged)then

            !--- Crop emerged
            n_ph_AG                 = n_ph_AG               + 1 ! Note: Here we consider that when leaf reach the surface the phytomer is above ground too. Depite the fact that most of its mass is below ground yet.
            n_lf_AG                 = n_lf_AG               + 1
            n_lf_alive_AG           = n_lf_alive_AG         + 1
            n_lf_alive_juveni_AG    = n_lf_alive_juveni_AG  + 1

            if(fl_stalk_emerged)then
                !--- Stalks Emerged
                n_it_AG =   n_it_AG + 1
            else
                !--- Stalks are below the ground
                n_it_BG =   n_it_BG + 1
            endif
        else

            !--- Crop is not emerged yet
            n_ph_BG                 = n_ph_BG               + 1
            n_it_BG                 = n_it_BG               + 1
            n_lf_BG                 = n_lf_BG               + 1
            n_lf_alive_BG           = n_lf_alive_BG         + 1
            n_lf_alive_juveni_BG    = n_lf_alive_juveni_BG  + 1
        endif

        !--- Update phytomer array
        do phy = n_ph, 1, -1

            !--- Update profile
            phprof(phy+1,1)   = phprof(phy,1)  ! Leaf Age
            phprof(phy+1,2)   = phprof(phy,2)  ! Leaf Sink strenght
            phprof(phy+1,3)   = phprof(phy,3)  ! Allocated Leaf biomass
            phprof(phy+1,4)   = phprof(phy,4)  ! Leaf area rate
            phprof(phy+1,5)   = phprof(phy,5)  ! Leaf area
            phprof(phy+1,6)   = phprof(phy,6)  ! Leaf weight
            phprof(phy+1,7)   = phprof(phy,7)  ! Internode Sink Strength dw rate g d-1
            phprof(phy+1,8)   = phprof(phy,8)  ! Initial Leaf Area [cm2]
            phprof(phy+1,9)   = phprof(phy,9)  ! Initial Leaf DW [g]
            phprof(phy+1,10)  = phprof(phy,10) ! Plant population at appearance
            phprof(phy+1,11)  = phprof(phy,11) ! Kmr Internode,
            phprof(phy+1,12)  = phprof(phy,12) ! Total phytomer Age
            phprof(phy+1,13)  = phprof(phy,13) ! Q10 Internode
            phprof(phy+1,14)  = phprof(phy,14) ! Internode Biomass at end of growth [g]
            phprof(phy+1,15)  = phprof(phy,15) ! Fiber Partitioning factor [0-1]
            phprof(phy+1,16)  = phprof(phy,16) ! Internode Length
            phprof(phy+1,17)  = phprof(phy,17) ! Fraction of Total Sugars
            phprof(phy+1,18)  = phprof(phy,18) ! Fraction of Fiber
            phprof(phy+1,19)  = phprof(phy,19) ! Fraction of Sucrose
            phprof(phy+1,20)  = phprof(phy,20) ! Fraction of Hexose
            phprof(phy+1,21)  = phprof(phy,21) ! Internode Growth Respiration
            phprof(phy+1,22)  = phprof(phy,22) ! Maintenance Respiration Factor (0-1) 1 =  is maintenance is ok
            phprof(phy+1,23)  = phprof(phy,23) ! dLength (cm)
            phprof(phy+1,24)  = phprof(phy,24) ! Lignin
            phprof(phy+1,25)  = phprof(phy,25) ! mresp leaf
            phprof(phy+1,26)  = phprof(phy,26) ! gresp leaf
            phprof(phy+1,27)  = phprof(phy,27) ! dw ss leaf
            phprof(phy+1,28)  = phprof(phy,28) ! sup_ratio_lf_phy
            phprof(phy+1,29)  = phprof(phy,29) ! supply_rate_lf
            phprof(phy+1,30)  = phprof(phy,30) ! supply_used_lf_phy
            phprof(phy+1,31)  = phprof(phy,31) ! supply_used_mresp_lf
            phprof(phy+1,32)  = phprof(phy,32) ! supply_used_gresp_lf
            phprof(phy+1,33)  = phprof(phy,33) ! supply_used_dw_lf
            phprof(phy+1,34)  = phprof(phy,34) ! maintenance_factor_lf
            phprof(phy+1,35)  = phprof(phy,35) ! reduc_growth_factor_lf
            phprof(phy+1,36)  = phprof(phy,36) ! mresp internode
            phprof(phy+1,37)  = phprof(phy,37) ! gresp internode
            phprof(phy+1,38)  = phprof(phy,38) ! dw ss internode
            phprof(phy+1,39)  = phprof(phy,39) ! sup_ratio_it_phy
            phprof(phy+1,40)  = phprof(phy,40) ! supply_rate_it_phy
            phprof(phy+1,41)  = phprof(phy,41) ! supply_used_it_phy
            phprof(phy+1,42)  = phprof(phy,42) ! supply_used_mresp_it_phy
            phprof(phy+1,43)  = phprof(phy,43) ! supply_used_gresp_it_phy
            phprof(phy+1,44)  = phprof(phy,44) ! supply_used_dw_it_phy
            phprof(phy+1,45)  = phprof(phy,45) ! maintenance_factor_it_phy
            phprof(phy+1,46)  = phprof(phy,46) ! reduc_growth_factor_it_phy
            phprof(phy+1,47)  = phprof(phy,47) ! Internode dry weigth rate [g dt-1]
            phprof(phy+1,48)  = phprof(phy,48) ! Internode structural dry weigth rate [g dt-1]
            phprof(phy+1,49)  = phprof(phy,49) ! Internode total sugars rate [g dt-1]
            phprof(phy+1,50)  = phprof(phy,50) ! Internode total dry weigth [g]
            phprof(phy+1,51)  = phprof(phy,51) ! Internode structural dry weigth [g]
            phprof(phy+1,52)  = phprof(phy,52) ! Internode total sugars [g]
            phprof(phy+1,53)  = phprof(phy,53) ! Internode sucrose weight [g]
            phprof(phy+1,54)  = phprof(phy,54) ! Internode hexoses weight [g]
            phprof(phy+1,55)  = phprof(phy,55) ! Leaf Age rate [dCdays]
            phprof(phy+1,56)  = phprof(phy,56) ! Phytomer Age rate [dCdays]
            phprof(phy+1,57)  = phprof(phy,57) ! Internode Age rate [dCdays]
            phprof(phy+1,58)  = phprof(phy,58) ! Internode Age [Cdays]

            !--- Flags Arrays
            fl_lf_AG(phy+1)   = fl_lf_AG(phy)
            fl_lf_alive(phy+1)= fl_lf_alive(phy)
            fl_it_AG(phy+1)   = fl_it_AG(phy)
        enddo

        !--- Reset phytomer stimuli
        phy_stimuli    = phy_stimuli  - 1.d0

        !-----------------------------------!
        !--- New phytomer initialization ---!
        !-----------------------------------!
        phprof(1, 1:60) = 0.d0 ! 60 is the total number of phytomers attributes nphy_att

        !--- Leaf is alive
        fl_lf_alive(1)  = .true.

        !--- Initial leaf area depending on leaf number appearance to deal with leaf sheath size at different stages
        ini_la  = min(1.d0, max(0.d0, (n_lf_AG - 1) / &
        & (n_lf_max_ini_la))) * (max_ini_la - init_leaf_area) + &
        & init_leaf_area

        !--- Leaf initial area and dry Weight [cm2 and g]
        if(flemerged) phprof(1,5)    = ini_la           ! [cm2]
        phprof(1,6)                  = ini_la / sla_sam     ! [g]

        !--- Store initial State for SS
        if(flemerged) phprof(1,8)    = ini_la           ! [cm2]
        phprof(1,9)                  = ini_la / sla_sam     ! [g]
        phprof(1,10)                 = nstk_now         ! [tiller m-2] For upscaling

        !--- Initial age of the new phytomer [cDays]
        phprof(1,1)  =   phy_stimuli * phyllochron
        phprof(1,12) =   phy_stimuli * plastochron
        phprof(1,58) =   0.d0   ! Internode will have age zero until reach "Natural Break Point"


        if(flemerged)then

            !--- Crop Emerged
            fl_lf_AG(1) = .true.

            if(fl_stalk_emerged)then
                !--- Stalks emerged
                fl_it_AG(1) = .true.
            else
                !--- Stalks below ground
                fl_it_AG(1) = .false.
            endif
        else
            !--- Crop not emerged
            fl_lf_AG(1) = .false.
            fl_it_AG(1) = .false.
        endif

        if(.not. fl_it_AG(1))then

            !--- New Below Ground Internode
            !--- Shared sugars [g] below ground sugars among below ground internodes
            shared_it_sug_BG =  (sug_it_BG * (1.e6/1.e4) / &
            & (nstk_now * tilleragefac)) / n_it_BG

            !--- Initialize below ground internode
            phprof(1,50) = shared_it_sug_BG                 ! All weight are sugars at initial step
            phprof(1,51) = 0.d0                             ! No structural
            phprof(1,52) = shared_it_sug_BG                 ! Total sugars
            phprof(1,53) = shared_it_sug_BG * frac_suc_BG   ! 50% share sucrose/hexose
            phprof(1,54) = shared_it_sug_BG * frac_hex_BG   ! 50% share sucrose/hexose
            phprof(1,14) = max_it_dw_BG
            phprof(1,11) = kmr_stor
            phprof(1,13) = q10_stor

            !--- Carbon balance including the new internode
            do phy = 2, n_it_BG

                !--- Carbon balance of reserves
                ts_it_phy       =   shared_it_sug_BG
                dw_it_phy       =   phprof(phy,51)  +   ts_it_phy

                !--- Update below ground internodes DW and Sugars
                phprof(phy,50)  =   dw_it_phy                       ! All weight are sugars at initial step
                phprof(phy,52)  =   ts_it_phy                       ! Total sugars
                phprof(phy,53)  =   shared_it_sug_BG * frac_suc_BG  ! 50% share sucrose/hexose
                phprof(phy,54)  =   shared_it_sug_BG * frac_hex_BG  ! 50% share sucrose/hexose
            enddo
        else
            phprof(1,14) = max_it_dw
            phprof(1,11) = kmr_stem
            phprof(1,13) = q10_stem
        endif

        !--- Appear a new young leaf
        fl_appear_leaf    = .true.

        !--- Leaf Maturity Counters
        dn_lf_alive_dewlap = 0

        !--- Amount of juvenile leaves
        if(n_lf_alive_juveni_AG .gt. (maxgl - maxdgl))then
            n_lf_alive_juveni_AG   = (maxgl - maxdgl)
            dn_lf_alive_dewlap  = 1
        endif

        !--- Number of Leaves with formed dewlap
        n_lf_alive_dewlap  = n_lf_alive_dewlap + dn_lf_alive_dewlap
        n_lf_AG_dewlap     = n_lf_AG_dewlap    + dn_lf_alive_dewlap

        !--- Check leaf spam
        if(n_lf_alive_dewlap .gt. maxdgl)then

            !--- Shed the oldest leaf
            fl_shed_leaf    = .true.

            !--- Update living leaf number
            n_lf_alive        = maxgl
            n_lf_alive_AG     = maxgl
            n_lf_alive_BG     = maxgl
            n_lf_alive_dewlap = maxdgl

        endif

        !--- Check if stalk emerged
        if(n_lf_AG .ge. n_lf_when_stk_emerg)then
            fl_stalk_emerged    = .true.
        endif

    endif

    !--- Before emergence
    if(.not. flemerged)then

        !--- Shoot Depth [cm]
        shootdepth  = shootdepth - dshootext_BG

        !--- Update Counter
        nphy_BGround = nphy_BGround + 1

        !--- Critical level of Substrates reserves
        !--- Not even maintenace respiration can be sustained under this condition
        if(sug_it_BG .le. 0.d0 .and. shootdepth .gt. 0.d0) then
            !--- Kill the crop before emergence
            flcropalive = .false.
            cropstatus  = '  Dead'
        endif

        if(shootdepth .le. 0.d0)then

            !------------------------!
            !--- Crop has emerged ---!
            !------------------------!
            flemerged = .true.
            !------------------------!

            !------------------------------------------!
            !--- Initialize above ground conditions ---!
            !------------------------------------------!
            shootdepth          = 0.d0
            phprof(n_ph,5)      = init_leaf_area                            ! Inital leaf area (cm2)
            nstk                = ini_nstk                                  ! Initial Tillering
            cropdstage          = 'Emergd'                                  ! Update Stage ID
            diac_at_emergence   = diacsoil                                  ! Cdays at Emergence
            lai                 = init_leaf_area * ini_nstk / 1.e4          ! [m2 m-2]
        endif
    endif


    !--------------------------!
    !--- Write Step Outputs ---!
    !--------------------------!

    !--- Detailed Photosynthesis output
    if(writedetphoto .and. flemerged)then

        !--- Convert to μmol m-2 s-1 for output purpose
        amax_out = amax_mod * 1.e3 / 1.e4 / 3600 / 44.d0 * 1.e6
        eff_out  = eff_mod  * 1.e3 / 1.e4 / 3600 / 44.d0 * 1.e6 / 4.6

        do glai = 1 ,5
            do ghour = 1, 3
                write(outdph,111) seqnow, ',', pltype, ',', year, &
                &  ',', doy, ',', das, ',', dap, ',', ghour, ',', glai, ',', &
                &  lai_ass, ',', frac_li, ',', amax_out, ',', eff_out, ',', &
                &  Acanopy(ghour+1,1), ',', Acanopy(1,glai+1), ',', &
                &  Qleaf(ghour+1,glai+1), ',', Acanopy(ghour+1,glai+1), ',', &
                &  incpar(ghour,2), ',', incpar(ghour,3), ',', incpar(ghour,4)
            enddo
        enddo
    endif

    write(outd,'(1x,i4,1x,i3,1x,20f20.5)') das, dap, soiltemperature, &
    &  di, li, dtg, dtga


111 format(     i2,         a1,       &  ! seqnow
    &            a6,         a1,      &   ! pltype
    &            i4,         a1,      &   ! year
    &            i3,         a1,      &   ! doy
    &            i4,         a1,      &   ! das
    &            i4,         a1,      &   ! dap
    &            i2,         a1,      &   ! ghour                 [hour of day]
    &            i2,         a1,      &   ! glai                  [canopy layer]
    &            f20.5,      a1,      &   ! lai                   [m2/m2]
    &            f20.5,      a1,      &   ! frac light absorbed   [0-1]
    &            f20.5,      a1,      &   ! amax_out              [μmol m-2 s-1]
    &            f20.5,      a1,      &   ! eff_out               [μmol(CO2) μmol(photon)-1]
    &            f20.5,      a1,      &   ! Acanopy(ghour)        [hour]
    &            f20.5,      a1,      &   ! Acanopy(glai)         [m2/m2]
    &            f20.5,      a1,      &   ! Qleaf(ghour,glai)     [μmol/m2/s]
    &            f20.5,      a1,      &   ! Acanopy(ghour,glai)   [μmol/m2/s]
    &            f20.5,      a1,      &   ! incpar(ghour,2)       [direct PAR - W/m2]
    &            f20.5,      a1,      &   ! incpar(ghour,3)       [difuse PAR - W/m2]
    &            f20.5)                 ! incpar(ghour,4)       [total PAR - W/m2]


    !--- Detailed Crop Outputs (Phytomer Profile)
    if(writedcrop)then
        do phy = 1, n_ph

            write(outdpa,113) seqnow, ',', pltype, ',', year, ',', doy, &
            &  ',', das, ',', dap, ',', phy, ',', fl_it_AG(phy), ',', &
            &  fl_lf_AG(phy), ',', fl_lf_alive(phy), ',', &
            &  'Leaf Age'                   , ',', 'Cdays'   , &
            &  ',', phprof(phy,1)

            write(outdpa,113) seqnow, ',', pltype, ',', year, ',', doy, &
            &  ',', das, ',', dap, ',', phy, ',', fl_it_AG(phy), ',', &
            &  fl_lf_AG(phy), ',', fl_lf_alive(phy), ',', &
            &  'Leaf Total DW'              , ',', 'g'       , &
            &  ',', phprof(phy,6)

            write(outdpa,113) seqnow, ',', pltype, ',', year, ',', doy, &
            &  ',', das, ',', dap, ',', phy, ',', fl_it_AG(phy), ',', &
            &  fl_lf_AG(phy), ',', fl_lf_alive(phy), ',', &
            &  'Leaf Area'                  , ',', 'cm2'     , &
            &  ',', phprof(phy,5)

            write(outdpa,113) seqnow, ',', pltype, ',', year, ',', doy, &
            &  ',', das, ',', dap, ',', phy, ',', fl_it_AG(phy), ',', &
            &  fl_lf_AG(phy), ',', fl_lf_alive(phy), ',', &
            &  'Internode Age'              , ',', 'Cdays'   , &
            &  ',', phprof(phy,58)

            write(outdpa,113) seqnow, ',', pltype, ',', year, ',', doy, &
            &  ',', das, ',', dap, ',', phy, ',', fl_it_AG(phy), ',', &
            &  fl_lf_AG(phy), ',', fl_lf_alive(phy), ',', &
            &  'Internode Total DW'         , ',', 'g'       , &
            &  ',', phprof(phy,50)

            write(outdpa,113) seqnow, ',', pltype, ',', year, ',', doy, &
            &  ',', das, ',', dap, ',', phy, ',', fl_it_AG(phy), ',', &
            &  fl_lf_AG(phy), ',', fl_lf_alive(phy), ',', &
            &  'Internode Structural DW'    , ',', 'g'       , &
            &  ',', phprof(phy,51)

            write(outdpa,113) seqnow, ',', pltype, ',', year, ',', doy, &
            &  ',', das, ',', dap, ',', phy, ',', fl_it_AG(phy), ',', &
            &  fl_lf_AG(phy), ',', fl_lf_alive(phy), ',', &
            &  'Internode Total Sugars DW'  , ',', 'g'       , &
            &  ',', phprof(phy,52)

            write(outdpa,113) seqnow, ',', pltype, ',', year, ',', doy, &
            &  ',', das, ',', dap, ',', phy, ',', fl_it_AG(phy), ',', &
            &  fl_lf_AG(phy), ',', fl_lf_alive(phy), ',', &
            &  'Internode Sucrose DW'       , ',', 'g'       , &
            &  ',', phprof(phy,53)

            write(outdpa,113) seqnow, ',', pltype, ',', year, ',', doy, &
            &  ',', das, ',', dap, ',', phy, ',', fl_it_AG(phy), ',', &
            &  fl_lf_AG(phy), ',', fl_lf_alive(phy), ',', &
            &  'Internode Hexose DW'        , ',', 'g'       , &
            &  ',', phprof(phy,54)

            write(outdpa,113) seqnow, ',', pltype, ',', year, ',', doy, &
            &  ',', das, ',', dap, ',', phy, ',', fl_it_AG(phy), ',', &
            &  fl_lf_AG(phy), ',', fl_lf_alive(phy), ',', &
            &  'Internode Length'           , ',', 'mm'      , &
            &  ',', phprof(phy,16)

            write(outdpa,113) seqnow, ',', pltype, ',', year, ',', doy, &
            &  ',', das, ',', dap, ',', phy, ',', fl_it_AG(phy), ',', &
            &  fl_lf_AG(phy), ',', fl_lf_alive(phy), ',', &
            &  'Phytomer Age'               , ',', 'Cdays'   , &
            &  ',', phprof(phy,12)

            write(outdpa,113) seqnow, ',', pltype, ',', year, ',', doy, &
            &  ',', das, ',', dap, ',', phy, ',', fl_it_AG(phy), ',', &
            &  fl_lf_AG(phy), ',', fl_lf_alive(phy), ',', &
            &  'Internode Fiber Fraction'   , ',', 'Cdays'   , &
            &  ',', phprof(phy,17)

            write(outdpa,113) seqnow, ',', pltype, ',', year, ',', doy, &
            &  ',', das, ',', dap, ',', phy, ',', fl_it_AG(phy), &
            &  ',', fl_lf_AG(phy), ',', fl_lf_alive(phy), ',', &
            &  'Internode Sugars Fraction'  , ',', 'Cdays'   , &
            &  ',', phprof(phy,18)

            write(outdpa,113) seqnow, ',', pltype, ',', year, ',', doy, &
            &  ',', das, ',', dap, ',', phy, ',', fl_it_AG(phy), ',', &
            &  fl_lf_AG(phy), ',', fl_lf_alive(phy), ',', &
            &  'Internode Sucrose Fraction' , ',', 'Cdays'   , &
            &  ',', phprof(phy,19)

            write(outdpa,113) seqnow, ',', pltype, ',', year, ',', doy, &
            &  ',', das, ',', dap, ',', phy, ',', fl_it_AG(phy), ',', &
            &  fl_lf_AG(phy), ',', fl_lf_alive(phy), ',', &
            &  'Internode Hexose Fraction'  , ',', 'Cdays'   , &
            &  ',', phprof(phy,20)

            write(outdpa,113) seqnow, ',', pltype, ',', year, ',', doy, &
            &  ',', das, ',', dap, ',', phy, ',', fl_it_AG(phy), &
            &  ',', fl_lf_AG(phy), ',', fl_lf_alive(phy), ',', &
            &  'Pfac Struc'                 , ',', '0-1'     , &
            &  ',', phprof(phy,15)

        enddo
113     format(i2,a1,a6,a1,i4,a1,i3,a1,i4,a1,i4,a1,i3, &
        & a1,l1,a1,l1,a1,l1,a1,a25,a1,a5,a1,f12.4)
    endif

    !--- Partitioning Factors Outputs
    write(outpfac, 144) das, dap, fl_use_reserves, &
    & cr_source_sink_ratio, dtcrss, tot_gresp_crop, &
    & tot_mresp_crop, tot_dw_ss_crop, &
    & dtg*(1.e6/1.e4), sug_it_BG, subs_avail_growth_crop, &
    & supply_used_crop, supply_used_mresp_crop, &
    & supply_used_gresp_crop, supply_used_dw_crop, &
    & reserves_used_mresp_crop, maintenance_factor_crop, &
    & reduc_growth_factor_crop, &
    & dr_rtss, dr_lfss, dr_itss, swfacp, str_it_AG, &
    & sug_it_AG, frac_li, li
144 format(1X,I4,3X,i3,3x,l1,3X,200F12.4)


    !--- Stress Factors Outputs
    write(outstres, 145) das, dap, trasw, eop, &
    & trwup*10.d0, max(trwup/(eop/10.),0.d0), swfacp, &
    & swface, swfacf, swfact, tmed, tempfac_pho, tempfac_per, &
    & co2, pho_fac_co2, diacem, agefactor_amax, &
    & agefactor_per, sug_it_BG, amaxfbfac, dtg*(1.e6/1.e4), per, ptra, peva, tra, reva

145 format(1X,I4,3X,i4,3x,200F12.4)
    !--------------------!
    !--- Crop Outputs ---!
    !--------------------!
    if(writeactout)then
        write(outp,109) seqnow, &
        & pltype, &
        & year, &
        & doy, &
        & das, &
        & dap, &
        & diac, &
        & dw_total, &
        & dw_it_AG, &
        & dw_lf, &
        & dw_rt, &
        & fw_it_AG, &
        & suc_it_AG, &
        & pol, &
        & lai, &
        & nstk, &
        & stk_h, &
        & n_lf_AG_dewlap*1., &
        &  swface, &
        &  swfacp, &
        & cropstatus, &
        & cropdstage, &
        & project

    endif

109 format(I2,3X,A6,3X,I4,4X,I3,3X,I4,4X,I3,1F8.1,2f8.2, &
    & 3F8.2,8F8.2,3X,A6,2X,A6,2X,A20)

    !--- Link with SWAP variables
    ch    =   stk_h * 1.e2    ! Crop Height [cm] for PenMon()
    cf    =   kc              ! Crop factor (-)

    !--- Roots cumulative density (Required by SWAP)
    if (cumdenstb .eq. 0) then
        call root_cumdens(numlay,rld,upper,bottom,cumdens)
    endif

    !--- write RLD and cumdens
    if(detailedsoil)then
        do sl = 1, numlay
            write(outds,51) &
            &           seqnow,                    ',', &
            &           year,                      ',', &
            &           doy,                       ',', &
            &           das,                       ',', &
            &           dap,                       ',', &
            &           rd,                        ',', &
            &           rld(sl),                   ',', &
            &           bottom(sl),                ',', &
            &           slthickness(sl),           ',', &
            &           sl


51          format( i2,     a1,    &  ! seqnow
            &        i4,     a1,   &  ! year
            &        i3,     a1,   &   ! doy
            &        i4,     a1,   &   ! das
            &        i4,     a1,   &   ! dap
            &        f10.5,   a1,  &   ! rd
            &        f10.5,   a1,  &   ! rld
            &        f10.5,   a1,  &   ! dp
            &        f10.5,   a1,  &   ! slthickness
            &        i3,     a1,   &   ! sl
            &        a28,    a1,   &   ! description
            &        a10,    a1,   &   ! units
            &        f10.5)          ! Layer variable

        enddo
    endif

    if(writecumdens)then
        do sl = 2, 202, 2
            write(outcumd,52) &
            &           seqnow,                    ',',  &
            &           year,                      ',', &
            &           doy,                       ',', &
            &           das,                       ',', &
            &           dap,                       ',', &
            &           rd,                        ',', &
            &           cumdens(sl - 1),           ',', &
            &           cumdens(sl)

52          format( i2,     a1,    &  ! seqnow
            &        i4,     a1,   &   ! year
            &        i3,     a1,   &   ! doy
            &        i4,     a1,   &   ! das
            &        i4,     a1,   &   ! dap
            &        f10.5,   a1,  &   ! rd
            &        f10.5,   a1,  &   ! cumdens
            &        f10.5,   a1,  &   ! cumdens
            &        f10.5,   a1,  &   ! slthickness
            &        i3,     a1,   &   ! sl
            &        a28,    a1,   &   ! description
            &        a10,    a1,   &   ! units
            &        f10.5)          ! Layer variable

        enddo
    endif


    if(flclos_file .and. flCropHarvest)then
        !--- close output files
        call output_samuca(  3, &
        &                    project, &
        &                    outp, &
        &                    outd, &
        &                    outdph, &
        &                    outdpa, &
        &                    outpfac, &
        &                    outstres, &
        &                    outds, &
        &                    outcumd, &
        &                    writedetphoto, &
        &                    writedcrop, &
        &                    writehead, &
        &                    detailedsoil, &
        &                    writecumdens)
    endif

    return

    end subroutine Samuca

    !************************************************************************

! ----------------------------------------------------------------------
      subroutine astro_sam &
     &   (logf,swscre,daynr,lat,dayl,daylp,sinld,cosld,dsinb,dsinbe_sam,dso)
! ----------------------------------------------------------------------
! Subroutine astro (daynr,lat,dayl,daylp,sinld,cosld)
! Authors: this routine Astro is based on Sastro (Daniel van Kraalingen)
! Date   : 28-November-2005 
! Purpose: This subroutine calculates solar constant, daily
!          extraterrestrial radiation, daylength and some intermediate
!          variables required by other routines. The routine has been
!          rewritten such that latitudes from pole to pole can be used.
!
! Formal parameters:  (I=input,O=output,C=control,IN=init,T=time)
! name   type meaning                                     units  class
! ----   ---- -------                                     -----  -----
! logf    I4  Internal number of logbook output file *.LOG   -      I
! swscre  I4  Switch of screen display:  0 = no display;     -      I
!             1 = summary water balance; 2 = daynumber
! daynr   I4  Day of year (Jan 1st = 1)                      d      I  
! lat     R8  Latitude of the site                       degrees    I  
! dayl    R8  Astronomical daylength (base = 0 degrees)      h      O  
! daylp   R8  Photoperiodic daylength (base = -4 degrees)    h      O  
! sinld   R8  Intermediate variable for other subroutine     -      O  
! cosld   R8  Intermediate variable for other subroutine     -      O  
! dsinb   R8  Daily total of sine of solar height            s      O  
! dsinbe  R8  Daily integral of sine of solar height         s      O  
!             corrected for lower transmission at low                  
!             elevation                                                
! sc      R8  Solar constant at day=daynr                   W/m2     O  
! dso     R8  Daily extraterrestrial radiation            J/m2/d    O  
!                                                                      
! Fatal error checks (on input): lat > 90, lat < -90
! Warnings          : lat above polar circle, lat within polar circle  
! Subprograms called: Warning
! File usage        : none
!----------------------------------------------------------------------
      implicit none
 
!     formal parameters
      integer logf,swscre,daynr
      real*8  lat,dayl,daylp,sinld,cosld,dsinb,dsinbe_sam,dso

!     local parameters
      real*8  angle,aob,dec,pi,rad,zza,zzcos,zzsin,sc,help1
      character messag*200

      data    pi /3.1415926d0/,angle /-4.0d0/
! ----------------------------------------------------------------------
! --- declination of the sun as a function of daynr
!     (see ref.manual: Radiation term: 23.45*rad=0.409 en (90-10)*rad=1.39)
      rad = pi/180.d0
      dec = -asin(sin(23.45d0*rad)*cos(2.d0*pi*dble(daynr+10)/365.0d0))
! --- some intermediate variables
      sinld = sin(rad*lat)*sin(dec)
      cosld = cos(rad*lat)*cos(dec)
      aob = sinld/cosld
! --- calculation of daylenght and photoperiodic daylength
!     solution for polar circle altutude adopted from 
!     Daniel van Kraalingen (routine Sastro, dd 12-june-1996,version 1.1)
      if (aob.lt.-1.0d0) then
        messag = 'Warning: latitude above polar circle, daylength= 0hrs'
        call warn ('Astro',messag,logf,swscre)
        dayl = 0.0d0
        zzcos =  0.0d0
        zzsin =  1.0d0
      else if (aob.gt.1.0d0) then
        messag = 'Warning: latitude within polar circle,daylength=24hrs'
        call warn ('Astro',messag,logf,swscre)
        dayl = 24.0d0
        zzcos =  0.0d0
        zzsin = -1.0d0
      else
        dayl  = 12.0d0*(1.0d0+2.0d0*asin(aob)/pi)
        help1 = (-sin(angle*rad)+sinld)/cosld
        if (help1.gt.1.0d0) then
          daylp = 24.0d0
        else
          daylp = 12.0d0*(1.0d0+2.0d0*asin(help1)/pi)
        endif
!        write(logf,*) 'help1=',help1,'daylp=',daylp
        zza   = pi*(12.0d0+dayl)/24.0d0
        zzcos = cos (zza)
        zzsin = sin (zza)
      endif

!     Daily integral of sine of solar height (DSINB) with a
!     correction for lower atmospheric transmission at lower solar
!     elevations (DSINBE)
      dsinb  = 2.0d0*3600.0d0*(dayl*0.50d0*sinld-12.0d0*cosld*zzcos/pi)
      dsinbe_sam = 2.0d0*3600.0d0*(dayl*(0.50d0*sinld+0.20d0*sinld**2.0d0+ &
     &      0.10d0*cosld**2.0d0)-(12.0d0*cosld*zzcos+ &
     &      9.6d0*sinld*cosld*zzcos+2.4d0*cosld**2.0d0*zzcos*zzsin)/pi)

!     Solar constant and daily extraterrestrial radiation
      sc = 1370.0d0*(1.0d0+0.033d0*cos (2.0d0*pi*daynr/365.d0))
      dso  = sc*dsinb

      return
      end
      
      
! ----------------------------------------------------------------------
      subroutine radiat (daynr,hour,dayl,sinld,cosld,avrad,sinb, &
      &                  pardir,pardif)
! ----------------------------------------------------------------------
! --- author: daniel van kraalingen, 1986
! --- calculates the fluxes of diffuse and direct photosynthetically
! --- active radiation from the total daily shortwave radiation actually
! --- received (avrad) for a given day of the year and hour of the day.
! --- the input variables dayl, sinld and cosld are calculated in astro.
! --- for more information: see spitters et al. (1988).
! ----------------------------------------------------------------------
      implicit none

      integer daynr

      real*8  aob,atmtr,avrad,cosld,dayl,dsinb,dsinbe,dso,frdif,hour
      real*8  par,pardif,pardir,pi,sc,sinb,sinld

      data    pi /3.1415926d0/
! ----------------------------------------------------------------------
! --- calculations on solar elevation
! --- sine of solar elevation sinb
      aob = sinld/cosld
      sinb = max (0.0d0,sinld+cosld*cos(2.0*pi*(hour+12.0d0)/24.0))
! --- integral of sinb
      dsinb = 3600.*(dayl*sinld+24.*cosld*sqrt(1.0d0-aob*aob)/pi)
! --- integral of sinb, corrected for lower atmospheric transmission
! --- at low solar elevations
      dsinbe = 3600.*(dayl*(sinld+0.4*(sinld*sinld+cosld*cosld*0.5))+&
     &         12.0*cosld*(2.0d0+3.0*0.4*sinld)*sqrt(1.0d0-aob*aob)/pi)

! --- solar constant and daily extraterrestrial radiation
      sc = 1370.*(1.0d0+0.033*cos(2.0*pi*daynr/365.))
      dso = sc*dsinb

! --- diffuse light fraction from atmospheric transmission
      atmtr = avrad/dso
      if (atmtr.gt.0.75d0) frdif = 0.23d0
      if (atmtr.le.0.75d0.and.atmtr.gt.0.35d0) frdif = 1.33d0-1.46*atmtr
      if (atmtr.le.0.35d0.and.atmtr.gt.0.07d0) &
     & frdif = 1.0d0-2.3*(atmtr-0.07d0)**2
      if (atmtr.le.0.07d0) frdif = 1.0d0

! --- photosynthetic active radiation, diffuse and direct
      par = 0.5*avrad*sinb*(1.0d0+0.4*sinb)/dsinbe
      pardif = min (par,sinb*frdif*atmtr*0.5*sc)
      pardir = par-pardif

      return
      end
      
      
         subroutine TempHour_samuca(tmaxday,tminday,doy,lat,a,b,c,thour)
        !Calculates the Hourly temperature based on Parton & Logan (1981)
    
        Implicit None

        integer hour
        integer doy
        real tsunset                               !Temperature related variables !oC
        real decsol                                ! astronomic variables
        real ahn                                   ! astronomic variables
        real timnight                              ! time related variables
        real timday                                ! time related variables
        real sunset
        real sunrise
        real photop
        real nigthp
        real bb
        real be
        real bbd_iday
        real bbd_inight
        real bbd_inight2
        real bbe
        real ddy
        real tmaxday
        real tminday
        real lat
        real d_2_r
        real r_2_d

        real a          !       = 1.607 Calibrated for Sao Paulo State   (original constants from Parton and Logan paper = 2.000)
        real b          !       = 2.762 Calibrated for Sao Paulo State   (original constants from Parton and Logan paper = 2.200)
        real c          !       = 1.179 Calibrated for Sao Paulo State   (original constants from Parton and Logan paper = -0.17)

        real thour(24)

        real :: pi      =  3.14159265

        save

        d_2_r = pi/180.
        r_2_d = 180./pi

        !calculating photoperiod
        decsol  = 23.45 * sin(((360./365.)*(doy-80.)*d_2_r))

        !ahn     = acos((-tan(d_2_r*lat)*tan(d_2_r*decsol)))
        photop  = acos((-tan((lat)*d_2_r))*(tan((decsol)*d_2_r))) * &
     & r_2_d * (2./15.)
        nigthp  = 24 - photop
        sunrise = 12 - photop/2
        sunset  = 12 + photop/2

        bb      = 12. - photop / 2. + c
        be      = 12. + photop / 2.
        ddy     = photop - c

        !Calculating air temperature follow Parton & Logan (1981)
        tsunset = (tmaxday - tminday) * sin(((pi*ddy)/(photop+2*a))) + &
     &  tminday

        !Initial Conditions
        do hour = 1,24

            bbd_iday    = hour - bb
            bbe         = hour - be
            bbd_inight2 = (24. + be) + hour

            !Rescaling time
            if(hour .gt. sunset) then
                bbd_inight  = hour - sunset
            else
                bbd_inight = (24. + hour) - sunset
            endif

            !Day time temperature
            if(hour .ge. bb .and. hour .le. sunset) then

                thour(hour) = (tmaxday - tminday) * &
     & sin(((pi * bbd_iday) / (photop + 2*a))) + tminday

            else
            !Night time temperature

                thour(hour) = tminday + (tsunset - tminday) * &
     & exp(-b * bbd_inight/(24. - photop))


            endif
        enddo
		
        return
        end subroutine TempHour_samuca