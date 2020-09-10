REPORT zrequest_report.

TABLES:
  tmsbufreq,
  tstrfcofil,
  e070,
  e070c,
  e07t,
  e070ctv.

TYPE-POOLS:
  ctslg, slis, icon, trwbo, trsel.

INCLUDE:
  rddkorri.

CLASS:
  lcl_progress_indicator DEFINITION DEFERRED.

**********************************************************************
*-------               CLASS DEFINITION                 -------------*
**********************************************************************
CLASS lcl_progress_indicator DEFINITION CREATE PUBLIC .

  PUBLIC SECTION.

* Interface required to serialize the object
    INTERFACES:
      if_serializable_object.

    EVENTS:
        show_message .

    CONSTANTS:
      co_ratio_percentage TYPE i VALUE 25 ##NO_TEXT,
      co_10_sec           TYPE i VALUE 10000000 ##NO_TEXT,
      co_1_sec            TYPE i VALUE 1000000 ##NO_TEXT.

    METHODS:
      show
        IMPORTING
          VALUE(im_v_text) TYPE any OPTIONAL
          im_v_processed   TYPE i OPTIONAL
          im_v_reset_procd TYPE abap_bool OPTIONAL
            PREFERRED PARAMETER im_v_processed,
      get_total RETURNING VALUE(r_result) TYPE i,
      set_total IMPORTING im_total TYPE i.

    CLASS-METHODS:
      show_msg_standalone
        IMPORTING
          im_v_text TYPE string OPTIONAL.

    METHODS:
      constructor
        IMPORTING
          im_v_total           TYPE i      OPTIONAL
          im_v_interval_in_sec TYPE i      OPTIONAL
          im_v_text_default    TYPE string OPTIONAL
            PREFERRED PARAMETER im_v_total ,
      reset_processed.

  PRIVATE SECTION.
    DATA:
      interval  TYPE i,
      rtime     TYPE int4,
      ratio     TYPE decfloat16,
      total     TYPE i,
      text      TYPE string,
      processed TYPE i.

    METHODS:
      calc_ratio.


ENDCLASS.

CLASS lcl_zrequest_report DEFINITION FINAL.

  PUBLIC SECTION.
    TYPES:
      BEGIN OF ty_s_alv,
        trkorr       TYPE trkorr,           " Request/Task
        trfunction   TYPE e070-trfunction,  " Type of request/task
        trfunctn_t   TYPE ko013-trfunctn_t, " Type (short text)
        client       TYPE trclient,         " Source client of request
        systemid     TYPE tarsystem,        " target system
        rc           TYPE strw_int4,        " return code
        rc_icon      TYPE icon_l4,          " return code
        trstatus     TYPE trstatus,         " Status of request/task
        trstatus_txt TYPE trstatus_t,       " Status of request/task Text
        date_step    TYPE as4date,          " Date Transport
        time_step    TYPE as4time,          " Time Transport
        as4user      TYPE trheader-as4user, " Owner of request/task
        as4text      TYPE as4text,          " Short Description of Repository Objects
        as4user_name TYPE ad_namtext,       " Full Name of Person
*        tarclient    TYPE e070c-tarclient,  " Target client for the request
        as4date      TYPE trheader-as4date, " Last Changed On
        as4time      TYPE trheader-as4time, " Last changed at
*        tardevcl     TYPE e070m-tardevcl,   " Target Package
*        devclass     TYPE e070m-devclass,   " Package
*        tarlayer     TYPE e070m-tarlayer,   " Target Transport Layer
      END OF ty_s_alv,

      ty_t_alv TYPE STANDARD TABLE OF ty_s_alv WITH KEY trkorr trfunction trfunctn_t systemid
                 WITH NON-UNIQUE SORTED KEY key1      COMPONENTS trkorr
                 WITH NON-UNIQUE SORTED KEY sort_key1 COMPONENTS date_step time_step trkorr trfunction trfunctn_t systemid,

      BEGIN OF ty_s_default_path,
        upload   TYPE string,
        download TYPE string,
      END OF ty_s_default_path.

    TYPES: BEGIN OF ty_s_tmscsys,
             alowed TYPE xfeld.
        INCLUDE TYPE tmscsys.
    TYPES END OF ty_s_tmscsys.

    TYPES ty_t_sys TYPE STANDARD TABLE OF ty_s_tmscsys  WITH KEY alowed domnam sysnam limbo.

    TYPES: BEGIN OF ty_s_conf,
             invers     TYPE tcevers-version,   " memory input version
             source     TYPE tcesyst-sysname,   " loaded systemconfig
             domname    TYPE tmscdom-domnam,
             trlayer    TYPE triwb_t_trlayer,   " transport layer
             laytext    TYPE triwb_t_laytext,
             system     TYPE triwb_t_system,
             system2    TYPE triwb_t_dsysts,    " system list
             systext    TYPE triwb_t_systext,
             release    TYPE triwb_t_release,   " new integration control
             deliver    TYPE triwb_t_deliver,   " deliveries
             target     TYPE triwb_t_target,    " targets
             tartext    TYPE triwb_t_tartext,
             clientc    TYPE triwb_t_clientc,   " client control
             dpltargets TYPE triwb_t_dpltarg,   " deploy targets
             version    TYPE triwb_s_version,   " versions
             vertext    TYPE triwb_t_vertext,
             tms_conf   TYPE tmsmconf,
             domains    TYPE tmscdom,
           END OF ty_s_conf,

           BEGIN OF ty_s_tr_files,
             filename       TYPE string,
             filetype       TYPE c LENGTH 1,
             filepath       TYPE string,
             server_path    TYPE eseiefile,
             filelength_bin TYPE i,
             header         TYPE xstring,
             data_tab       TYPE esy_tt_rcgrepfile, "rcgrepfile.
             text_buffer    TYPE string,
             filelength_asc TYPE i,
           END OF ty_s_tr_files,
           ty_t_tr_files TYPE STANDARD TABLE OF ty_s_tr_files WITH KEY filename,
           BEGIN OF ty_s_tr_import,
             trkorr        TYPE trkorr,           " Request/Task
             trfunction    TYPE e070-trfunction,  " Type of request/task
             trfunctn_t    TYPE ko013-trfunctn_t, " Type (short text)
             client        TYPE trclient,         " Source client of request
             srcsystem     TYPE srcsystem,        " Original System of Object
             tarsystem     TYPE tarsystem,        " target system
             date_step     TYPE as4date,          " Date Transport
             time_step     TYPE as4time,          " Time Transport
             as4user       TYPE trheader-as4user, " Owner of request/task
             t_tr_filedata TYPE ty_t_tr_files,
           END OF ty_s_tr_import,

           ty_t_tr_import  TYPE STANDARD TABLE OF ty_s_tr_import WITH KEY trkorr,
           ty_t_file_table TYPE STANDARD TABLE OF file_info WITH DEFAULT KEY.

    CONSTANTS:
*      cc_development_sys TYPE tarsystem VALUE 'S4D',
      cc_quality_sys    TYPE tarsystem VALUE 'S4Q',
      cc_production_sys TYPE tarsystem VALUE 'S4P',
      BEGIN OF gc_trstatus,
        modifiable      TYPE trstatus VALUE 'D',        " D Modifiable
        modifiable_prot TYPE trstatus VALUE 'L',        " L Modifiable, Protected
        release_started TYPE trstatus VALUE 'O',        " O Release Started
        released        TYPE trstatus VALUE 'R',        " R Released
        released_repair TYPE trstatus VALUE 'N',        " N Released (with Import Protection for Repaired Objects)
      END OF gc_trstatus,

      BEGIN OF gc_trfunction,
        customizing       TYPE trfunction VALUE 'W',    " W Customizing Request
        workbench         TYPE trfunction VALUE 'K',    " K Workbench Request
        transport_copies  TYPE trfunction VALUE 'T',    " T Transport of Copies
        relocation        TYPE trfunction VALUE 'C',    " C Relocation of Objects Without Package Change
        relocation_packch TYPE trfunction VALUE 'O',    " O Relocation of Objects with Package Change
        relocation_compl  TYPE trfunction VALUE 'E',    " E Relocation of complete package
      END OF gc_trfunction,

      BEGIN OF gc_task_function,
        customizing       TYPE trfunction VALUE 'S',    " S   Development/Correction
        workbench         TYPE trfunction VALUE 'R',    " R   Repair
        unclassified_task TYPE trfunction VALUE 'X',    " X   Unclassified Task
        relocation        TYPE trfunction VALUE 'Q',    " Q   Customizing Task
        relocation_packch TYPE trfunction VALUE 'G',    " G   Piece List for CTS Project
        relocation_compl  TYPE trfunction VALUE 'M',    " M   Client Transport Request
        pl_upgrade        TYPE trfunction VALUE 'P',    " P   Piece List for Upgrade
        pl_support_pack   TYPE trfunction VALUE 'D',    " D   Piece List for Support Package
        piece_list        TYPE trfunction VALUE 'F',    " F   Piece List
        deletion          TYPE trfunction VALUE 'L',    " L   Deletion transport
      END OF gc_task_function,

      BEGIN OF gc_tr_file,
        data_path     TYPE c LENGTH 4 VALUE 'data',
        data_prefix   TYPE c LENGTH 1 VALUE 'R',
        cofiles_path  TYPE c LENGTH 7 VALUE 'cofiles',
        cofile_prefix TYPE c LENGTH 1 VALUE 'K',
      END OF gc_tr_file,

      BEGIN OF gc_log_stepid,
        import                TYPE trbatfunc VALUE 'I',
        dictionary_activation TYPE trbatfunc VALUE 'A', " Dictionary Activation
        import_ended          TYPE trbatfunc VALUE '!', " Import ended
        waiting_qa_approval   TYPE trbatfunc VALUE 'p', " Request waiting for QA approval
        qa_approval_given     TYPE trbatfunc VALUE 'q', " QA approval given
      END OF gc_log_stepid.

    DATA:
      gt_alv       TYPE ty_t_alv.

*--------------------------------------------------------------------*
*-------------         METHODS PUBLIC SECTION           -------------*
*--------------------------------------------------------------------*
    METHODS:
      constructor,
      trint_select_requests
        IMPORTING
          im_v_refresh TYPE abap_bool OPTIONAL,
      user_command
        IMPORTING
          VALUE(im_v_ucomm) TYPE syst_ucomm,
      alv_show.

  PROTECTED SECTION.

  PRIVATE SECTION.

    DATA:
      go_alv_table       TYPE REF TO cl_salv_table,
      go_alv_top_of_list TYPE REF TO cl_salv_form_layout_grid,
*      go_alv_flow_num_rec TYPE REF TO cl_salv_form_layout_flow,
*      go_alv_flow_date    TYPE REF TO cl_salv_form_layout_flow,
      go_msg_ind         TYPE REF TO lcl_progress_indicator,
      gt_requests        TYPE trwbo_request_headers,
      gs_selection       TYPE trwbo_selection,
      gs_ranges          TYPE trsel_ts_ranges,
      gs_default_path    TYPE ty_s_default_path,
      gt_sys             TYPE ty_t_sys,
      gs_conf            TYPE ty_s_conf.

*--------------------------------------------------------------------*
*-------------        METHODS PRIVATE SECTION           -------------*
*--------------------------------------------------------------------*
    METHODS:
      data_retrieval,
      delete_system_not_final
        IMPORTING
          im_v_system TYPE tarsystem,

      on_alv_link_click     FOR EVENT link_click OF cl_salv_events_table
        IMPORTING
            row
            column,

      on_alv_user_command   FOR EVENT if_salv_events_functions~added_function  OF cl_salv_events_table
        IMPORTING
            e_salv_function,
      on_alv_double_click   FOR EVENT double_click OF cl_salv_events_table
        IMPORTING
            row
            column,
      alv_create,
      alv_set_icon_type
        CHANGING
          ch_s_alv TYPE lcl_zrequest_report=>ty_s_alv,
      alv_set_tr_text
        CHANGING
          ch_s_alv TYPE lcl_zrequest_report=>ty_s_alv,
      alv_set_columns,
      call_rddit076,
      tr_download,
      tr_import,
      file_upload_binary,
      tr_import_upload_files
        IMPORTING
          im_v_filename        TYPE file_info-filename
          im_v_selected_folder TYPE string
        CHANGING
          ch_s_tr_import       TYPE ty_s_tr_import,
      tr_import_select_files
        IMPORTING
          im_t_file_table      TYPE ty_t_file_table
          im_v_selected_folder TYPE string
        CHANGING
          ch_t_tr_import       TYPE ty_t_tr_import,
      tr_import_save_files_on_unix
        IMPORTING
          im_t_tr_import TYPE ty_t_tr_import,
      tr_import_zip_upload_files
        IMPORTING
          im_v_filename        TYPE file_info-filename
          im_v_selected_folder TYPE string
        CHANGING
          ch_t_tr_import       TYPE ty_t_tr_import.

ENDCLASS.

DATA: go_request TYPE REF TO lcl_zrequest_report.

INITIALIZATION.

  CREATE OBJECT go_request.

CLASS lcl_zrequest_report IMPLEMENTATION.

  METHOD constructor.

    DATA: lt_sys             TYPE STANDARD TABLE OF tmscsys  WITH KEY domnam sysnam limbo.

    " Read configuration from system
    CALL FUNCTION 'TMS_CFG_READ_CONFIGURATION'
      EXPORTING
        iv_only_active = abap_true
        iv_all_systems = abap_true
      IMPORTING
        es_conf        = gs_conf-tms_conf
        es_dom         = gs_conf-domains
      TABLES
        tt_sys         = lt_sys           " System Description
      EXCEPTIONS
        OTHERS         = 12.

    IF sy-subrc NE 0.
      MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
        WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
    ENDIF.

    CALL FUNCTION 'TRINT_TCE_READ_CONFIG'
      EXPORTING
        iv_language   = syst-langu
      IMPORTING
        et_vertext    = gs_conf-vertext
        es_version    = gs_conf-version
        et_system     = gs_conf-system
        et_trlayer    = gs_conf-trlayer
        et_release    = gs_conf-release
        et_deliver    = gs_conf-deliver
        et_systext    = gs_conf-systext
        et_laytext    = gs_conf-laytext
        et_tartext    = gs_conf-tartext
        et_target     = gs_conf-target
        et_clientc    = gs_conf-clientc
        et_dpltargets = gs_conf-dpltargets
      EXCEPTIONS
        OTHERS        = 3.
    IF sy-subrc NE 0.
      MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
        WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
    ENDIF.






    MOVE-CORRESPONDING lt_sys TO me->gt_sys.

    cl_gui_frontend_services=>get_upload_download_path(
      CHANGING
        upload_path                 = gs_default_path-upload    " Upload Path
        download_path               = gs_default_path-download  " Download Path
      EXCEPTIONS
        OTHERS                      = 6               ).

    me->alv_create(  ).

    me->trint_select_requests(  ).

    me->alv_show(  ).

  ENDMETHOD.

**********************************************************************
*-------            TRINT_SELECT_REQUESTS               -------------*
**********************************************************************
  METHOD trint_select_requests.
    DATA:
    lv_via_selscreen TYPE abap_bool VALUE abap_on.
*K   ordem de workbench
*W   Ordem customizing
*C   Relocação objetos s/mudança de pacote
*O   Relocação de objetos c/mudança de pacote
*E   Relocação de um pacote completo
*T   Transporte de cópias
*S   Desenvolvimento/correção
*R   Reparação
*X   Tarefa não classificada
*Q   Tarefa customizing
*G   Lista técnica para projeto CTS
*M   Ordem para transporte de mandante
*P   Lista técnica para upgrade
*D   Lista técnica para patch
*F   Lista técnica
*L   Transporte de eliminação

    " Identify Refresh command
    IF im_v_refresh EQ abap_on.
      lv_via_selscreen = abap_off.
    ENDIF.

    CLEAR: me->gt_requests, me->gt_alv.

    CALL FUNCTION 'TR_SYS_PARAMS'
      IMPORTING
        systemname = system_name
        systemtype = system_type.

    gs_selection-reqfunctions       = gc_trfunction.
    gs_selection-reqstatus          = gc_trstatus.
    gs_selection-trkorrpattern(3)   = system_name(3).
    gs_selection-trkorrpattern+3(2) = 'K*'.

    " Selection of Requests
    CALL FUNCTION 'TRINT_SELECT_REQUESTS'
      EXPORTING
        iv_username_pattern    = sy-uname                           " User selecting requests
        is_selection           = me->gs_selection                   " Other request/task parameters
        iv_complete_projects   = abap_on                            " 'X': STRKORR relationships are traced
        iv_via_selscreen       = lv_via_selscreen
        iv_title               = 'Lista de status de requests'(001) " Title for selection screen
      IMPORTING
        et_requests            = me->gt_requests                    " Table
      CHANGING
        cs_ranges              = me->gs_ranges
      EXCEPTIONS
        action_aborted_by_user = 1
        OTHERS                 = 2.

    IF sy-subrc EQ 1.
      LEAVE PROGRAM.
    ELSEIF sy-subrc NE 0.
      MESSAGE ID sy-msgid TYPE 'S' NUMBER sy-msgno DISPLAY LIKE 'E'
                 WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
      RETURN.
    ENDIF.

    CALL METHOD data_retrieval.

  ENDMETHOD.

**********************************************************************
*-------                DATA_RETRIEVAL                  -------------*
**********************************************************************
  METHOD data_retrieval.

    DATA:
      ls_cofile  TYPE ctslg_cofile,
      ls_request TYPE trwbo_request,
      ls_alv     TYPE ty_s_alv.

    CREATE OBJECT me->go_msg_ind
      EXPORTING
        im_v_total = lines( me->gt_requests ).

    LOOP AT me->gt_requests ASSIGNING FIELD-SYMBOL(<fs_requests>) WHERE trfunction IN me->gs_ranges-request_funcs.

      CLEAR: ls_alv, ls_request, ls_cofile.

      me->go_msg_ind->show( im_v_text = |Lendo informações Request: { <fs_requests>-trkorr }| ).

      CALL FUNCTION 'TR_READ_REQUEST'
        EXPORTING
          iv_trkorr          = <fs_requests>-trkorr " Request number
          iv_read_e070       = abap_on      "  Read request header information
          iv_read_e07t       = abap_on      "  Read request short text
          iv_read_e070c      = abap_on      "  Read request client
          iv_read_e070m      = abap_on      "  Read request client
*         iv_read_objs_keys  = abap_on      "  Read objects and key of the request
          iv_read_attributes = abap_on      "  Read request attributes
        CHANGING
          cs_request         = ls_request   " Complete request
        EXCEPTIONS
          OTHERS             = 1.

      CALL FUNCTION 'TR_READ_GLOBAL_INFO_OF_REQUEST'
        EXPORTING
          iv_trkorr = <fs_requests>-trkorr  " Request number
        IMPORTING
          es_cofile = ls_cofile.

      IF <fs_requests>-trstatus   EQ gc_trstatus-modifiable OR "D  Modificável
         <fs_requests>-trfunction CA gc_task_function .        " Task

        MOVE-CORRESPONDING:
            <fs_requests> TO ls_alv,
            ls_request-h  TO ls_alv.
*        ls_alv-client   = ls_request-h-client.
        ls_alv-systemid = sy-sysid.
        ls_alv-rc       = 99.

        me->alv_set_tr_text( CHANGING ch_s_alv = ls_alv ).

        APPEND ls_alv TO me->gt_alv.

      ENDIF.

      LOOP AT ls_cofile-systems     ASSIGNING FIELD-SYMBOL(<fs_system>).
        LOOP AT <fs_system>-steps   ASSIGNING FIELD-SYMBOL(<fs_step>) WHERE stepid EQ gc_log_stepid-import.

          ls_alv-client   = ls_request-h-client.
          ls_alv-systemid = <fs_system>-systemid.
          ls_alv-rc       = <fs_system>-rc.

          LOOP AT <fs_step>-actions ASSIGNING FIELD-SYMBOL(<fs_action>). " action
            ls_alv-date_step = <fs_action>-date.
            ls_alv-time_step = <fs_action>-time.
          ENDLOOP.

          MOVE-CORRESPONDING:
            <fs_requests> TO ls_alv,
            ls_request-h  TO ls_alv.

          me->alv_set_icon_type(
           CHANGING
             ch_s_alv = ls_alv ).

          me->alv_set_tr_text(
            CHANGING
              ch_s_alv = ls_alv ).

          APPEND ls_alv TO me->gt_alv.

        ENDLOOP.
      ENDLOOP.
    ENDLOOP.

    DELETE ADJACENT DUPLICATES FROM me->gt_alv COMPARING trkorr systemid.

    me->delete_system_not_final( me->cc_production_sys ).
    me->delete_system_not_final( me->cc_quality_sys ).

    SORT me->gt_alv  DESCENDING BY date_step time_step .


  ENDMETHOD.

**********************************************************************
*-------            DELETE_SYSTEM_NOT_FINAL             -------------*
**********************************************************************
  METHOD delete_system_not_final.

    DATA:
    lt_alv TYPE TABLE OF ty_s_alv.

    MOVE me->gt_alv TO lt_alv.

    LOOP AT lt_alv ASSIGNING FIELD-SYMBOL(<fs_s_alv>) WHERE systemid = im_v_system.
      DELETE me->gt_alv WHERE trkorr   EQ <fs_s_alv>-trkorr
                          AND systemid NE <fs_s_alv>-systemid.
    ENDLOOP.

  ENDMETHOD.


  METHOD alv_show.
    DATA:
      lo_logo              TYPE REF TO cl_salv_form_layout_logo.

    go_alv_top_of_list->create_flow( row = 1 column  = 1 )->create_label( text = 'Sistema:'(017) ).
    go_alv_top_of_list->create_flow( row = 1 column  = 2 )->create_text(  text = |{ syst-sysid } { syst-mandt }| ).

    go_alv_top_of_list->create_flow( row = 2 column  = 1 )->create_label( text = 'Usuário:'(016) ).
    go_alv_top_of_list->create_flow( row = 2 column  = 2 )->create_text(  text = |{ syst-uname }| ).

    go_alv_top_of_list->create_flow( row = 3 column  = 1 )->create_label( text = |{ 'Data de execução:'(015) }| ).
    go_alv_top_of_list->create_flow( row = 3 column  = 2 )->create_text(  text = |{ sy-datlo DATE = USER } { syst-uzeit TIME = USER }| ).

    go_alv_top_of_list->create_flow( row = 4 column  = 1 )->create_label( text = 'Número total de registros selecionados: '(014) ).
    go_alv_top_of_list->create_flow( row = 4 column  = 2 )->create_text(  text = lines( me->gt_alv ) ).

    CREATE OBJECT lo_logo.

* set left content
    lo_logo->set_left_content( go_alv_top_of_list ).

* set Right Image
    lo_logo->set_right_logo( 'ZTAESA_LOGO_ALV' ).

*   set the top of list using the header for Online.
    me->go_alv_table->set_top_of_list( lo_logo ).

    me->go_alv_table->display(  ).
    me->user_command( syst-ucomm ).

  ENDMETHOD.

  METHOD on_alv_user_command.
    me->user_command( e_salv_function ).
  ENDMETHOD.

  METHOD user_command.

    CLEAR: syst-ucomm.

    CASE im_v_ucomm.
      WHEN '%EX'.
        LEAVE TO LIST-PROCESSING.

      WHEN '&F03'. "Back
        CALL METHOD:
            me->trint_select_requests( ),
            me->alv_show.

      WHEN '&F15' OR '&F12'. " Exit or Cancel
        LEAVE PROGRAM.

      WHEN 'REFRESH'.
        me->trint_select_requests( im_v_refresh = abap_on ).
        me->go_alv_table->refresh(  ).

      WHEN 'RDDIT076'.
        me->call_rddit076( ).

      WHEN 'TR_EXPORT'.
        me->tr_download( ).

      WHEN 'TR_IMPORT'.
        me->tr_import( ).

    ENDCASE.

  ENDMETHOD.

  METHOD on_alv_link_click.

    READ TABLE me->gt_alv INDEX row ASSIGNING FIELD-SYMBOL(<row_alv>).

    CASE column.

      WHEN 'TRKORR'.

        CALL FUNCTION 'TR_PRESENT_REQUEST'
          EXPORTING
            iv_trkorr = <row_alv>-trkorr.

      WHEN 'RC_ICON'.

        CALL FUNCTION 'TR_LOG_OVERVIEW_REQUEST_REMOTE'
          EXPORTING
            iv_trkorr = <row_alv>-trkorr.

      WHEN 'AS4USER'.

        CALL FUNCTION 'SUSR_SHOW_USER_DETAILS'
          EXPORTING
            bname = <row_alv>-as4user.

    ENDCASE.
  ENDMETHOD.

  METHOD on_alv_double_click.

    READ TABLE me->gt_alv INDEX row ASSIGNING FIELD-SYMBOL(<row_alv>).

    CASE column.

      WHEN 'AS4USER'.

        CALL FUNCTION 'SUSR_SHOW_USER_DETAILS'
          EXPORTING
            bname = <row_alv>-as4user.

      WHEN OTHERS.

        CALL FUNCTION 'TR_DISPLAY_REQUEST'
          EXPORTING
            i_trkorr    = <row_alv>-trkorr    " Request/Task
            i_operation = 'DISPLAY'.

    ENDCASE.

  ENDMETHOD.

  METHOD alv_create.

    DATA:
      lo_events     TYPE REF TO cl_salv_events_table,
      ls_layout_key TYPE salv_s_layout_key.

    TRY.

        " Create object ALV TABLE
        CALL METHOD cl_salv_table=>factory
          IMPORTING
            r_salv_table = me->go_alv_table " Basis Class Simple ALV Tables
          CHANGING
            t_table      = me->gt_alv.

        CALL METHOD me->go_alv_table->set_screen_status
          EXPORTING
            report        = syst-repid                          " ABAP Program: Current Master Program
            pfstatus      = 'ALV_STATUS'                        " Screens, Current GUI Status
            set_functions = me->go_alv_table->c_functions_all.  " ALV: Data Element for Constants

        me->go_alv_table->get_functions( )->set_all( abap_true ).

        " Set Display Settings
        me->go_alv_table->get_display_settings( )->set_horizontal_lines( abap_true ).
        me->go_alv_table->get_display_settings( )->set_striped_pattern( abap_true ).

        " Set Functional Settings
        me->go_alv_table->get_functional_settings( )->set_sort_on_header_click( abap_true ).

        " Set Selections Type
        me->go_alv_table->get_selections( )->set_selection_mode( if_salv_c_selection_mode=>row_column ).

        " Set Columns Parameters
        me->alv_set_columns( ).

        " Set Layout
        ls_layout_key-report = sy-repid.
        me->go_alv_table->get_layout( )->set_key( ls_layout_key ).
        me->go_alv_table->get_layout( )->set_save_restriction( if_salv_c_layout=>restrict_none ).

        DATA(lv_has_layout_default) = me->go_alv_table->get_layout( )->has_default( ).
        DATA(salv_s_layout)         = me->go_alv_table->get_layout( )->get_default_layout( ).

        CREATE OBJECT go_alv_top_of_list
          EXPORTING
            columns = 3.
        me->go_alv_table->set_top_of_list( value = go_alv_top_of_list ).

*        me->alv_create_header_and_footer( ).

        "events
        lo_events = me->go_alv_table->get_event( ).
        SET HANDLER me->on_alv_link_click       FOR lo_events.
        SET HANDLER me->on_alv_double_click     FOR lo_events.
        SET HANDLER me->on_alv_user_command     FOR lo_events.

      CATCH cx_root INTO DATA(lcx_root).
        " Deu ruim!
        MESSAGE lcx_root->get_longtext( ) TYPE 'E'.

    ENDTRY.

  ENDMETHOD.


  METHOD alv_set_icon_type.

    " Set Icon type
    CASE ch_s_alv-rc.
      WHEN 0.                               "Sucess
        ch_s_alv-rc_icon = icon_led_green.
      WHEN 4 OR 6 OR 1204.                  "Warning
        ch_s_alv-rc_icon = icon_led_yellow.
      WHEN 8.                               "Error
        ch_s_alv-rc_icon = icon_led_red.
      WHEN 12 OR 13 OR 14 OR 16.            "Cancel
        ch_s_alv-rc_icon = icon_message_critical.
      WHEN 99.
        ch_s_alv-rc_icon = icon_space.
    ENDCASE.

  ENDMETHOD.


  METHOD alv_set_tr_text.
    DATA:
    ls_user_display TYPE soud2.

    CALL FUNCTION 'TR_REQUEST_TYPE_TEXT'
      EXPORTING
        iv_request_type        = ch_s_alv-trfunction
        iv_request_status      = ch_s_alv-trstatus
      IMPORTING
        ev_request_type_text   = ch_s_alv-trfunctn_t
        ev_request_status_text = ch_s_alv-trstatus_txt.

    CALL FUNCTION 'SO_USER_READ'
      EXPORTING
        user_name    = ch_s_alv-as4user " SAPoffice name of user
      IMPORTING
        user_display = ls_user_display  " Data division of user record (can be displayed)
      EXCEPTIONS
        OTHERS       = 4.

    ch_s_alv-as4user_name = ls_user_display-adrname.

  ENDMETHOD.


  METHOD alv_set_columns.
    DATA:
      lo_alv_columns    TYPE REF TO cl_salv_columns,
      lo_alv_colum      TYPE REF TO cl_salv_column_table,
      lt_components     TYPE cl_abap_structdescr=>component_table,
      lt_ddfields       TYPE ddfields,
      ls_ddic_reference TYPE salv_s_ddic_reference.

    TRY.
        lo_alv_columns ?= me->go_alv_table->get_columns( ).
        lo_alv_columns->set_optimize( abap_true ).

        zcl_utils=>get_field_list(
          EXPORTING
            im_t_table      = me->gt_alv
          IMPORTING
            ex_t_components = lt_components
          RECEIVING
            r_result        = lt_ddfields        ).

        LOOP AT lt_components ASSIGNING FIELD-SYMBOL(<component>).

          lo_alv_colum ?= lo_alv_columns->get_column( columnname = |{ <component>-name }| ).
          lo_alv_colum->set_alignment( value = if_salv_c_alignment=>centered ).

          CASE <component>-name.

            WHEN 'TRKORR'.
              lo_alv_colum->set_cell_type( if_salv_c_cell_type=>hotspot ).
              lo_alv_colum->set_key( abap_on ).
*              lo_alv_colum->set_optimized( abap_false ).
*              lo_alv_colum->set_output_length( 12 ).

            WHEN 'TRFUNCTION' OR 'RC' OR 'TRSTATUS'.
              lo_alv_colum->set_technical( abap_true ).

            WHEN 'TRFUNCTN_T'.
              ls_ddic_reference-field = 'TRFUNCTION'.
              ls_ddic_reference-table = 'E070'.
              lo_alv_colum->set_ddic_reference( ls_ddic_reference ).

            WHEN 'RC_ICON'.
              lo_alv_colum->set_cell_type( if_salv_c_cell_type=>hotspot ).
              lo_alv_colum->set_long_text( 'Log Erro' ).
              lo_alv_colum->set_medium_text( 'Log Erro' ).
              lo_alv_colum->set_short_text( 'Log Erro' ).

            WHEN 'TRSTATUS_TXT'.
              ls_ddic_reference-field = 'TRSTATUS'.
              ls_ddic_reference-table = 'E070'.
              lo_alv_colum->set_ddic_reference( ls_ddic_reference ).
*              lo_alv_colum->set_output_length( 10 ).

            WHEN 'date'.
              lo_alv_colum->set_cell_type( if_salv_c_cell_type=>hotspot ).
              lo_alv_colum->set_long_text( 'Log Erro' ).
              lo_alv_colum->set_medium_text( 'Log Erro' ).
              lo_alv_colum->set_short_text( 'Log Erro' ).


*            WHEN 'AS4USER'.
*              lo_alv_colum->set_optimized( abap_false ).
*              lo_alv_colum->set_output_length( 10 ).

            WHEN 'AS4USER_NAME' OR 'AS4TEXT'.
              lo_alv_colum->set_alignment( value = if_salv_c_alignment=>left ).

            WHEN OTHERS.
              lo_alv_colum->set_optimized( abap_true ).

          ENDCASE.


        ENDLOOP.
      CATCH cx_root INTO DATA(lcx_root).
        MESSAGE lcx_root->get_text( ) TYPE 'E'.
    ENDTRY.

  ENDMETHOD.


  METHOD call_rddit076.

    DATA:
        lt_rows TYPE salv_t_row.

    lt_rows  = me->go_alv_table->get_selections( )->get_selected_rows( ).

    LOOP AT lt_rows ASSIGNING FIELD-SYMBOL(<row>).
      READ TABLE me->gt_alv ASSIGNING FIELD-SYMBOL(<alv>) INDEX <row>.
      IF syst-subrc IS INITIAL.

        CALL FUNCTION 'TRINT_DISPLAY_REQUEST_HEADER'
          EXPORTING
            iv_read_only = abap_off
            iv_trkorr    = <alv>-trkorr
          EXCEPTIONS
            OTHERS       = 5.

        IF sy-subrc NE 0.
          MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
            WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
        ENDIF.

      ENDIF.

    ENDLOOP.

  ENDMETHOD.

  METHOD tr_download.

    DATA:
      lt_rows            TYPE salv_t_row,
      lv_dir_trans       TYPE char255,
      lv_selected_folder TYPE string,
      lv_window_title    TYPE string,
      lb_check_dir       TYPE abap_bool,
      lv_server_path     TYPE sapb-sappfad,
      lv_targetpath      TYPE sapb-sappfad,
      lv_tr_filename     TYPE string.

    lt_rows  = me->go_alv_table->get_selections( )->get_selected_rows( ).

    IF lt_rows IS INITIAL.
      MESSAGE 'No requests selected'(018) TYPE 'S' DISPLAY LIKE 'E'.
      RETURN.
    ENDIF.

    " Get default path for requests /usr/sap/trans
    CALL 'C_SAPGPARAM' ID 'NAME' FIELD 'DIR_TRANS' ID 'VALUE' FIELD lv_dir_trans.

    lv_selected_folder = me->gs_default_path-download.
    lv_window_title = 'Select a folder to download requests'(020).

    cl_gui_frontend_services=>directory_browse(
      EXPORTING
        window_title         = lv_window_title      " Title of Browsing Window
        initial_folder       = lv_selected_folder   " Start Browsing Here
      CHANGING
        selected_folder      = lv_selected_folder   " Folder Selected By User
      EXCEPTIONS
        OTHERS               = 4   ).

    IF syst-subrc IS NOT INITIAL.
      MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
                 WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
      RETURN.
    ENDIF.

    IF lv_selected_folder IS INITIAL.
      MESSAGE 'Download canceled'(019) TYPE 'S' DISPLAY LIKE 'E'.
      RETURN.
    ENDIF.

    cl_gui_frontend_services=>directory_exist(
      EXPORTING
        directory            = lv_selected_folder   " Directory name
      RECEIVING
        result               = lb_check_dir         " Result
      EXCEPTIONS
        OTHERS               = 5    ).
    IF sy-subrc IS NOT INITIAL OR lb_check_dir NE abap_true.
      MESSAGE ID sy-msgid TYPE 'S' NUMBER sy-msgno
        WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4 DISPLAY LIKE 'E'.
      RETURN.
    ENDIF.

    LOOP AT lt_rows ASSIGNING FIELD-SYMBOL(<row>).
      READ TABLE me->gt_alv ASSIGNING FIELD-SYMBOL(<alv>) INDEX <row>.

      IF  <alv>-trstatus NE gc_trstatus-released.
        MESSAGE ID 'TR' TYPE 'I' NUMBER 492 WITH <alv>-trkorr. "  Request &1 has not been released
        EXIT FROM STEP-LOOP.
      ENDIF.

      DO 2 TIMES.

        lv_tr_filename      = |{ <alv>-trkorr+4(6) }.{ <alv>-trkorr(3) }|.

        IF syst-index EQ 1.
          lv_server_path    =    gc_tr_file-data_path.
          lv_tr_filename    = |{ gc_tr_file-data_prefix }{ lv_tr_filename }|.   " R908261.SHD
        ELSE.
          lv_server_path    =    gc_tr_file-cofiles_path.
          lv_tr_filename    = |{ gc_tr_file-cofile_prefix }{ lv_tr_filename }|. " K908261.SHD
        ENDIF.

        lv_server_path      = |{ lv_dir_trans }/{ lv_server_path }/{ lv_tr_filename }|.

        lv_targetpath       = |{ lv_selected_folder }\\{ <alv>-trkorr }\\{ lv_tr_filename }|.

        CALL FUNCTION 'ARCHIVFILE_SERVER_TO_CLIENT'
          EXPORTING
            path       = lv_server_path
            targetpath = lv_targetpath
          EXCEPTIONS
            OTHERS     = 2.

        IF sy-subrc NE 0.
          MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
                  WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
          EXIT. " exit do
        ENDIF.
      ENDDO.
    ENDLOOP.

  ENDMETHOD.

  METHOD tr_import.

    CONSTANTS: lc_fileformat_binary TYPE rlgrap-filetype VALUE 'BIN'.

    DATA:
      lv_selected_folder TYPE string,
      lv_sub_folder      TYPE string,
      lv_upload_path     TYPE string,
      lv_window_title    TYPE string,
      lt_file_table      TYPE STANDARD TABLE OF file_info WITH DEFAULT KEY,
      lt_sub_file_table  TYPE STANDARD TABLE OF file_info WITH DEFAULT KEY,
      lv_file_count      TYPE i,
      lt_tr_import       TYPE STANDARD TABLE OF ty_s_tr_import WITH KEY trkorr,
      lo_alv_table       TYPE REF TO cl_salv_table.

    lv_upload_path  = me->gs_default_path-upload.
    lv_window_title = 'Select a folder to upload requests'(020).

    cl_gui_frontend_services=>directory_browse(
      EXPORTING
        window_title         = lv_window_title      " Title of Browsing Window
        initial_folder       = lv_upload_path       " Start Browsing Here
      CHANGING
        selected_folder      = lv_selected_folder   " Folder Selected By User
      EXCEPTIONS
        OTHERS               = 4   ).

    IF syst-subrc IS NOT INITIAL.
      MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
                 WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
      RETURN.
    ENDIF.

    IF lv_selected_folder IS INITIAL.
      MESSAGE 'Upload canceled' TYPE 'S' DISPLAY LIKE 'E'.
      RETURN.
    ENDIF.

    cl_gui_frontend_services=>directory_list_files(
      EXPORTING
        directory                   = lv_selected_folder    " Directory To Search
      CHANGING
        file_table                  = lt_file_table         " Return Table for the Found Files
        count                       = lv_file_count         " Number of Files/Dir Found
      EXCEPTIONS
        OTHERS                      = 6    ).

    IF sy-subrc IS NOT INITIAL.
      MESSAGE ID sy-msgid TYPE 'S' NUMBER sy-msgno
        WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4 DISPLAY LIKE 'E'.
      RETURN.
    ENDIF.


    " Pergunta se deve procurar em subpastas, se for encontrado
    READ TABLE lt_file_table WITH KEY isdir = '1' BINARY SEARCH TRANSPORTING NO FIELDS.
    IF syst-subrc IS INITIAL.
      " tratamento aqui!
    ENDIF.

    me->tr_import_select_files(
        EXPORTING
            im_t_file_table      = lt_file_table
            im_v_selected_folder = lv_selected_folder
        CHANGING
            ch_t_tr_import = lt_tr_import ).


    LOOP AT lt_file_table ASSIGNING FIELD-SYMBOL(<ls_file_table>) WHERE isdir EQ '1'.

      CLEAR: lt_sub_file_table, lv_sub_folder.

      lv_sub_folder  = |{ lv_selected_folder }\\{ <ls_file_table>-filename }|.

      cl_gui_frontend_services=>directory_list_files(
           EXPORTING
             directory                   = lv_sub_folder
           CHANGING
             file_table                  = lt_sub_file_table     " Return Table for the Found Files
             count                       = lv_file_count         " Number of Files/Dir Found
           EXCEPTIONS
             OTHERS                      = 6    ).

      me->tr_import_select_files(
          EXPORTING
              im_t_file_table      = lt_sub_file_table
              im_v_selected_folder = lv_sub_folder
          CHANGING
              ch_t_tr_import = lt_tr_import ).

    ENDLOOP.

    DATA:
      ls_layout_key TYPE salv_s_layout_key.

    TRY.

        " Create object ALV TABLE
        CALL METHOD cl_salv_table=>factory
          IMPORTING
            r_salv_table = lo_alv_table " Basis Class Simple ALV Tables
          CHANGING
            t_table      = lt_tr_import.

*        CALL METHOD me->go_alv_table->set_screen_status
*          EXPORTING
*            report        = syst-repid                          " ABAP Program: Current Master Program
*            pfstatus      = 'ALV_STATUS'                        " Screens, Current GUI Status
*            set_functions = me->go_alv_table->c_functions_all.  " ALV: Data Element for Constants

        lo_alv_table->get_functions( )->set_all( abap_true ).

        " Set Display Settings
        lo_alv_table->get_display_settings( )->set_horizontal_lines( abap_true ).
        lo_alv_table->get_display_settings( )->set_striped_pattern( abap_true ).

        " Set Functional Settings
        lo_alv_table->get_functional_settings( )->set_sort_on_header_click( abap_true ).

        " Set Selections Type
        lo_alv_table->get_selections( )->set_selection_mode( if_salv_c_selection_mode=>row_column ).

        " Set Columns Parameters
        me->alv_set_columns( ).

        " Set Layout
        ls_layout_key-report = sy-repid.
        lo_alv_table->get_layout( )->set_key( ls_layout_key ).
        lo_alv_table->get_layout( )->set_save_restriction( if_salv_c_layout=>restrict_none ).


        lo_alv_table->set_screen_popup(
          EXPORTING
            start_column = 1
            end_column   = 100
            start_line   = 1
            end_line     = lines( lt_tr_import ) + 2 ).

        lo_alv_table->display( ).


      CATCH cx_root INTO DATA(lcx_root).
        " Deu ruim!
        MESSAGE lcx_root->get_longtext( ) TYPE 'E'.

    ENDTRY.

    me->tr_import_save_files_on_unix( im_t_tr_import = lt_tr_import ).

  ENDMETHOD.

  METHOD file_upload_binary.

  ENDMETHOD.


  METHOD tr_import_upload_files.

    CONSTANTS: lc_fileformat_binary TYPE rlgrap-filetype VALUE 'BIN'.

    DATA:
      lv_filename  TYPE string VALUE space,
      lv_srcsystem TYPE srcsystem,        " Original System of Object
      lv_trkorr    TYPE trkorr,
      lv_dir_trans TYPE char255.

    " Get default path for requests /usr/sap/trans
    CALL 'C_SAPGPARAM' ID 'NAME' FIELD 'DIR_TRANS' ID 'VALUE' FIELD lv_dir_trans.

    SPLIT im_v_filename AT '.' INTO lv_trkorr lv_srcsystem.

    ch_s_tr_import-trkorr = |{ lv_srcsystem }{ lv_trkorr }|.

    DO 2 TIMES.

      APPEND INITIAL LINE TO ch_s_tr_import-t_tr_filedata ASSIGNING FIELD-SYMBOL(<ls_tr_filedata>).
      lv_filename = im_v_filename.
      <ls_tr_filedata>-filetype     = gc_tr_file-cofile_prefix.
      <ls_tr_filedata>-server_path  = |{ lv_dir_trans }/{ gc_tr_file-cofiles_path }/{ lv_filename }|.

      IF syst-index EQ '2'.
        " Replace K for R, change COFILE for DATA request file
        REPLACE me->gc_tr_file-cofile_prefix IN lv_filename WITH me->gc_tr_file-data_prefix.
        <ls_tr_filedata>-filetype       = gc_tr_file-data_prefix.
        <ls_tr_filedata>-server_path    = |{ lv_dir_trans }/{ gc_tr_file-data_path }/{ lv_filename }|.
      ENDIF.

      <ls_tr_filedata>-filename  = lv_filename.
      <ls_tr_filedata>-filepath  = im_v_selected_folder.
      lv_filename                = |{ im_v_selected_folder }{ lv_filename }|.

      cl_gui_frontend_services=>gui_upload(
        EXPORTING
          filename                = lv_filename                     " Name of file
          filetype                = |{ lc_fileformat_binary }|      " File Type (ASCII, Binary)
        IMPORTING
          filelength              = <ls_tr_filedata>-filelength_bin " File Length
          header                  = <ls_tr_filedata>-header         " File Header in Case of Binary Upload
        CHANGING
          data_tab                = <ls_tr_filedata>-data_tab       " Transfer table for file contents
        EXCEPTIONS
          OTHERS                  = 19       ).
      IF sy-subrc NE 0.
        MESSAGE ID sy-msgid TYPE 'S' NUMBER sy-msgno
          WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
        RETURN.
      ENDIF.

      IF syst-index EQ '1'.
        CALL FUNCTION 'SCMS_BINARY_TO_STRING'
          EXPORTING
            input_length  = <ls_tr_filedata>-filelength_bin
          IMPORTING
            text_buffer   = <ls_tr_filedata>-text_buffer
            output_length = <ls_tr_filedata>-filelength_asc
          TABLES
            binary_tab    = <ls_tr_filedata>-data_tab
          EXCEPTIONS
            OTHERS        = 2.
        IF sy-subrc NE 0.
          MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
                WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
        ENDIF.

        ch_s_tr_import-as4user    = <ls_tr_filedata>-text_buffer(12).
        ch_s_tr_import-trfunction = <ls_tr_filedata>-text_buffer+13(1).
        ch_s_tr_import-tarsystem  = <ls_tr_filedata>-text_buffer+15(10).

        SPLIT <ls_tr_filedata>-text_buffer AT cl_abap_char_utilities=>newline INTO TABLE DATA(lt_segments).

        " Get Line 4 with log start
        ASSIGN lt_segments[ 4 ] TO FIELD-SYMBOL(<ls_segments>).

        " Get Source System
        ch_s_tr_import-srcsystem = <ls_segments>(3).

        " Split line to get timestemp
        SPLIT <ls_segments> AT space INTO TABLE DATA(lt_log_line_segments).

        ASSIGN lt_log_line_segments[ 4 ] TO FIELD-SYMBOL(<ls_log_line_segments>).

        ch_s_tr_import-date_step = <ls_log_line_segments>(8).
        ch_s_tr_import-time_step = <ls_log_line_segments>+8(6).

      ENDIF.

    ENDDO.

  ENDMETHOD.

  METHOD tr_import_zip_upload_files.

    CONSTANTS: lc_fileformat_binary TYPE rlgrap-filetype VALUE 'BIN'.

    DATA:
      lv_filename  TYPE string VALUE space,
      lv_srcsystem TYPE srcsystem,        " Original System of Object
      lv_trkorr    TYPE trkorr,
      lv_dir_trans TYPE char255,
      lv_xhead     TYPE xstring,
      lv_content   TYPE xstring,
      lo_zip       TYPE REF TO cl_abap_zip.

    " Get default path for requests /usr/sap/trans
    CALL 'C_SAPGPARAM' ID 'NAME' FIELD 'DIR_TRANS' ID 'VALUE' FIELD lv_dir_trans.

    " Get File Type
    FIND ALL OCCURRENCES OF '.'
      IN im_v_filename
      IN CHARACTER MODE
      MATCH OFFSET DATA(lv_moff).

    CREATE OBJECT lo_zip.

    APPEND INITIAL LINE TO ch_t_tr_import               ASSIGNING FIELD-SYMBOL(<ls_tr_import>).
    APPEND INITIAL LINE TO <ls_tr_import>-t_tr_filedata ASSIGNING FIELD-SYMBOL(<ls_tr_filedata>).

    lv_filename = im_v_filename.

    <ls_tr_filedata>-filename  = lv_filename.
    <ls_tr_filedata>-filepath  = im_v_selected_folder.
    lv_filename                = |{ im_v_selected_folder }{ lv_filename }|.

    cl_gui_frontend_services=>gui_upload(
      EXPORTING
        filename                = lv_filename                     " Name of file
        filetype                = |{ lc_fileformat_binary }|      " File Type (ASCII, Binary)
      IMPORTING
        filelength              = <ls_tr_filedata>-filelength_bin " File Length
        header                  = <ls_tr_filedata>-header         " File Header in Case of Binary Upload
      CHANGING
        data_tab                = <ls_tr_filedata>-data_tab       " Transfer table for file contents
      EXCEPTIONS
        OTHERS                  = 19       ).
    IF sy-subrc NE 0.
      MESSAGE ID sy-msgid TYPE 'S' NUMBER sy-msgno
        WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
      RETURN.
    ENDIF.

    CALL FUNCTION 'SCMS_BINARY_TO_XSTRING'
      EXPORTING
        input_length = <ls_tr_filedata>-filelength_bin
      IMPORTING
        buffer       = lv_xhead
      TABLES
        binary_tab   = <ls_tr_filedata>-data_tab
      EXCEPTIONS
        OTHERS       = 2.
    IF sy-subrc NE 0.
      MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
            WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
    ENDIF.

    lo_zip->load( lv_xhead ).

    LOOP AT lo_zip->files ASSIGNING FIELD-SYMBOL(<ls_zip_file>).

      IF <ls_zip_file>-name(1) NE gc_tr_file-cofile_prefix. " K
        EXIT.
      ENDIF.

      SPLIT <ls_zip_file>-name AT '.' INTO lv_trkorr lv_srcsystem.
      <ls_tr_import>-trkorr = |{ lv_srcsystem }{ lv_trkorr }|.

      DO 2 TIMES.

        lv_filename                   = <ls_zip_file>-name.
        <ls_tr_filedata>-filetype     = gc_tr_file-cofile_prefix.   " K
        <ls_tr_filedata>-server_path  = |{ lv_dir_trans }/{ gc_tr_file-cofiles_path }/{ lv_filename }|.

        IF syst-index EQ '2'.
          APPEND INITIAL LINE TO <ls_tr_import>-t_tr_filedata ASSIGNING <ls_tr_filedata>.
          " Replace K for R, change COFILE for DATA request file
          REPLACE me->gc_tr_file-cofile_prefix IN lv_filename WITH me->gc_tr_file-data_prefix.
          <ls_tr_filedata>-filetype       = gc_tr_file-data_prefix.
          <ls_tr_filedata>-server_path    = |{ lv_dir_trans }/{ gc_tr_file-data_path }/{ lv_filename }|.
        ENDIF.

        lo_zip->get(
          EXPORTING
          name                      = lv_filename   " Name (Case-Sensitive)
          IMPORTING
            content                 = lv_content    " Contents
          EXCEPTIONS
            OTHERS                  = 3           ).
        IF sy-subrc NE 0.
          MESSAGE ID sy-msgid TYPE 'S' NUMBER sy-msgno
            WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
          RETURN.
        ENDIF.

        CALL FUNCTION 'SCMS_XSTRING_TO_BINARY'
          EXPORTING
            buffer        = lv_content
          IMPORTING
            output_length = <ls_tr_filedata>-filelength_bin
          TABLES
            binary_tab    = <ls_tr_filedata>-data_tab.


        IF syst-index EQ '1'.
          CALL FUNCTION 'SCMS_BINARY_TO_STRING'
            EXPORTING
              input_length  = <ls_tr_filedata>-filelength_bin
            IMPORTING
              text_buffer   = <ls_tr_filedata>-text_buffer
              output_length = <ls_tr_filedata>-filelength_asc
            TABLES
              binary_tab    = <ls_tr_filedata>-data_tab
            EXCEPTIONS
              OTHERS        = 2.
          IF sy-subrc NE 0.
            MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
                  WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
          ENDIF.

          <ls_tr_import>-as4user    = <ls_tr_filedata>-text_buffer(12).
          <ls_tr_import>-trfunction = <ls_tr_filedata>-text_buffer+13(1).
          <ls_tr_import>-tarsystem  = <ls_tr_filedata>-text_buffer+15(10).

          SPLIT <ls_tr_filedata>-text_buffer AT cl_abap_char_utilities=>newline INTO TABLE DATA(lt_segments).

          " Get Line 4 with log start
          ASSIGN lt_segments[ 4 ] TO FIELD-SYMBOL(<ls_segments>).

          " Get Source System
          <ls_tr_import>-srcsystem = <ls_segments>(3).

          " Split line to get timestemp
          SPLIT <ls_segments> AT space INTO TABLE DATA(lt_log_line_segments).

          ASSIGN lt_log_line_segments[ 4 ] TO FIELD-SYMBOL(<ls_log_line_segments>).

          <ls_tr_import>-date_step = <ls_log_line_segments>(8).
          <ls_tr_import>-time_step = <ls_log_line_segments>+8(6).

        ENDIF.


      ENDDO.
    ENDLOOP.

  ENDMETHOD.

  METHOD tr_import_select_files.
    DATA: lv_selected_folder TYPE string.

    lv_selected_folder = |{ im_v_selected_folder }\\|. " Insert \ at last

    LOOP AT im_t_file_table ASSIGNING FIELD-SYMBOL(<ls_file_info>) WHERE isdir       NE '1' AND
                                                                         filename(1) EQ gc_tr_file-cofile_prefix. " K
      APPEND INITIAL LINE TO ch_t_tr_import ASSIGNING FIELD-SYMBOL(<ls_tr_import>).

      me->tr_import_upload_files(
          EXPORTING
            im_v_filename        = <ls_file_info>-filename
            im_v_selected_folder = lv_selected_folder
          CHANGING
            ch_s_tr_import       = <ls_tr_import> ).

    ENDLOOP.

    LOOP AT im_t_file_table ASSIGNING <ls_file_info> WHERE isdir    NE '1' AND
                                                        filename    CS '.ZIP'.

      me->tr_import_zip_upload_files(
          EXPORTING
            im_v_filename        = <ls_file_info>-filename
            im_v_selected_folder = lv_selected_folder
          CHANGING
            ch_t_tr_import       = ch_t_tr_import ).

    ENDLOOP.

  ENDMETHOD.

  METHOD tr_import_save_files_on_unix.

    DATA:
      lv_lines         TYPE i,
      lv_dir_trans     TYPE char255,
      lv_max_len       TYPE i  VALUE 2550,
      lv_filename      TYPE authb-filename,
      lv_len           TYPE i,
      lv_all_lines_len TYPE i,
      lv_diff_len      TYPE i.


    LOOP AT im_t_tr_import ASSIGNING FIELD-SYMBOL(<ls_tr_import>).
      LOOP AT <ls_tr_import>-t_tr_filedata ASSIGNING FIELD-SYMBOL(<ls_tr_filedata>).

* count lines in rawdata table
        DESCRIBE TABLE <ls_tr_filedata>-data_tab LINES lv_lines.

        lv_filename = <ls_tr_filedata>-server_path.

* open dataset for writing

        CATCH SYSTEM-EXCEPTIONS open_dataset_no_authority = 1
                                dataset_too_many_files = 2
                                OTHERS = 4.
          OPEN DATASET <ls_tr_filedata>-server_path FOR OUTPUT IN BINARY MODE.
        ENDCATCH.

        IF sy-subrc IS NOT INITIAL.
*          RAISE open_failed.
        ELSE.

          lv_len = lv_max_len.

          LOOP AT <ls_tr_filedata>-data_tab ASSIGNING FIELD-SYMBOL(<ls_data_tab>).
*     last line is shorter perhaps
            IF sy-tabix = lv_lines.
              lv_all_lines_len   = lv_max_len * ( lv_lines - 1 ).
              lv_diff_len       = <ls_tr_filedata>-filelength_bin - lv_all_lines_len.
              lv_len            = lv_diff_len.
            ENDIF.

*     write data in file
            CATCH SYSTEM-EXCEPTIONS dataset_write_error = 1
                                                 OTHERS = 4.
              TRANSFER <ls_data_tab> TO <ls_tr_filedata>-server_path LENGTH lv_len.
            ENDCATCH.
            IF sy-subrc IS NOT INITIAL.
*              RAISE write_failed.
            ENDIF.
          ENDLOOP.
        ENDIF.

* close the dataset
        CATCH SYSTEM-EXCEPTIONS dataset_cant_close = 1
                                OTHERS = 4.
          CLOSE DATASET <ls_tr_filedata>-server_path.
        ENDCATCH.

        IF sy-subrc IS NOT INITIAL.
*          RAISE close_failed.
        ENDIF.

      ENDLOOP.
    ENDLOOP.


  ENDMETHOD.

ENDCLASS.

CLASS lcl_progress_indicator IMPLEMENTATION.

  METHOD constructor.

    " Total of Interactions
    me->total = im_v_total.

    " Calculate ratio
    IF me->total IS NOT INITIAL.
      me->calc_ratio( ).
    ENDIF.

    " Define interval to show message progress
    IF im_v_interval_in_sec IS NOT INITIAL.
      me->interval = im_v_interval_in_sec * co_1_sec.
    ELSE.
      me->interval = co_10_sec.
    ENDIF.

    IF im_v_text_default IS NOT INITIAL.
      me->text = im_v_text_default.
    ELSE.
      me->text = 'Processando:  '(001).
    ENDIF.

  ENDMETHOD.


  METHOD show.

    DATA:
      lv_text      TYPE string,
      lv_output_i  TYPE boole_d VALUE IS INITIAL,
      lv_rtime     TYPE int4,
      lv_processed TYPE i.

    TRY.

        IF im_v_reset_procd IS NOT INITIAL.
          me->reset_processed( ).
        ENDIF.

        ADD 1 TO me->processed.

        IF im_v_processed IS NOT INITIAL.
          lv_processed = im_v_processed.
        ELSE.
          lv_processed = me->processed.
        ENDIF.

        " Get RunTime!
        GET RUN TIME FIELD lv_rtime.

        IF im_v_text IS NOT INITIAL.
          lv_text = |{ im_v_text }|.
        ELSE.
          lv_text = me->text.
        ENDIF.

        IF me->total IS NOT INITIAL.
          lv_text = | { lv_text } [{ lv_processed }{ ' de '(002) } { me->total }] |.
        ENDIF.

        IF me->ratio IS NOT INITIAL.
          DATA(lv_result_mod) = lv_processed MOD me->ratio.
        ELSE.
          lv_result_mod = 1.
        ENDIF.

*   Displays the message every 25% OR every 10sec or First times
        IF ( lv_processed LT 4 )      OR
             lv_result_mod IS INITIAL OR
             ( lv_rtime - me->rtime ) GT me->interval.
          lv_output_i = abap_true.
        ENDIF.

        CALL METHOD cl_progress_indicator=>progress_indicate
          EXPORTING
            i_text               = lv_text          " Progress Text (If no message transferred in I_MSG*)
            i_processed          = lv_processed     " Number of Objects Already Processed
            i_total              = me->total        " Total Number of Objects to Be Processed
            i_output_immediately = lv_output_i.     " X = Display Progress Immediately

        me->rtime = lv_rtime.

      CATCH cx_root.
    ENDTRY.

  ENDMETHOD.


  METHOD get_total.
    r_result = me->total.
  ENDMETHOD.

  METHOD set_total.
    me->total = im_total.
    CALL METHOD me->calc_ratio.
  ENDMETHOD.

  METHOD reset_processed.
    CLEAR me->processed.
  ENDMETHOD.

  METHOD calc_ratio.
    me->ratio = ceil( me->total / co_ratio_percentage ).
  ENDMETHOD.

  METHOD show_msg_standalone.

    CALL METHOD cl_progress_indicator=>progress_indicate
      EXPORTING
        i_text               = im_v_text          " Progress Text (If no message transferred in I_MSG*)
        i_processed          = 99     " Number of Objects Already Processed
        i_total              = 100        " Total Number of Objects to Be Processed
        i_output_immediately = abap_on.     " X = Display Progress Immediately

  ENDMETHOD.

ENDCLASS.
