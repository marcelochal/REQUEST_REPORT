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

**********************************************************************
*-------               CLASS DEFINITION                 -------------*
**********************************************************************
CLASS lcl_zrequest_report DEFINITION FINAL.

  PUBLIC SECTION.
    TYPES:
      BEGIN OF ty_s_alv,
        trkorr       TYPE trkorr,
        trfunction   TYPE e070-trfunction,
        trfunctn_t   TYPE ko013-trfunctn_t,
        client       TYPE trclient,
        systemid     TYPE tarsystem,  " target system
        rc           TYPE strw_int4,  " return code
*        rc_icon      TYPE tp_icon,    " return code
        rc_icon      TYPE icon_l4,    " return code
        trstatus     TYPE trstatus,
        trstatus_txt TYPE trstatus_t,
        date         TYPE as4date,
        time         TYPE as4time,
        as4user      TYPE trheader-as4user,
        as4text      TYPE as4text,
        as4user_name TYPE ad_namtext,
*        tarclient    TYPE e070c-tarclient,
        as4date      TYPE trheader-as4date,
        as4time      TYPE trheader-as4time,
*        tardevcl     TYPE e070m-tardevcl,
*        devclass     TYPE e070m-devclass,
*        tarlayer     TYPE e070m-tarlayer,
      END OF ty_s_alv,

      ty_t_alv TYPE STANDARD TABLE OF ty_s_alv WITH KEY trkorr trfunction trfunctn_t systemid
                 WITH NON-UNIQUE SORTED KEY key1      COMPONENTS trkorr
                 WITH NON-UNIQUE SORTED KEY sort_key1 COMPONENTS date time trkorr trfunction trfunctn_t systemid.

    CONSTANTS:
*      cc_development_sys TYPE tarsystem VALUE 'S4D',
      cc_quality_sys    TYPE tarsystem VALUE 'S4Q',
      cc_production_sys TYPE tarsystem VALUE 'S4P',
      BEGIN OF co_trstatus,
        modifiable      TYPE trstatus VALUE 'D',    " D Modifiable
        modifiable_prot TYPE trstatus VALUE 'L',    " L   Modifiable, Protected
        release_started TYPE trstatus VALUE 'O',    " O   Release Started
        released        TYPE trstatus VALUE 'R',    " R   Released
        released_repair TYPE trstatus VALUE 'N',    " N   Released (with Import Protection for Repaired Objects)
      END OF co_trstatus,
      BEGIN OF co_trfunction,
        customizing       TYPE trfunction VALUE 'W',    " W Customizing Request
        workbench         TYPE trfunction VALUE 'K',    " K Workbench Request
        transport_copies  TYPE trfunction VALUE 'T',    " T Transport of Copies
        relocation        TYPE trfunction VALUE 'C',    " C Relocation of Objects Without Package Change
        relocation_packch TYPE trfunction VALUE 'O',    " O Relocation of Objects with Package Change
        relocation_compl  TYPE trfunction VALUE 'E',    " E Relocation of complete package
      END OF co_trfunction,
      BEGIN OF co_task_function,
        customizing       TYPE trfunction VALUE 'S',    " S   Development/Correction
        workbench         TYPE trfunction VALUE 'R',    " R   Repair
        transport_copies  TYPE trfunction VALUE 'X',    " X   Unclassified Task
        relocation        TYPE trfunction VALUE 'Q',    " Q   Customizing Task
        relocation_packch TYPE trfunction VALUE 'G',    " G   Piece List for CTS Project
        relocation_compl  TYPE trfunction VALUE 'M',    " M   Client Transport Request
        pl_upgrade        TYPE trfunction VALUE 'P',    " P   Piece List for Upgrade
        pl_support_pack   TYPE trfunction VALUE 'D',    " D   Piece List for Support Package
        piece_list        TYPE trfunction VALUE 'F',    " F   Piece List
        deletion          TYPE trfunction VALUE 'L',    " L   Deletion transport
      END OF co_task_function.

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
      go_alv_table        TYPE REF TO cl_salv_table,
      go_alv_layout       TYPE REF TO cl_salv_layout,
      go_alv_top_of_list  TYPE REF TO cl_salv_form_layout_grid,
      go_alv_flow_num_rec TYPE REF TO cl_salv_form_layout_flow,
      go_alv_flow_date    TYPE REF TO cl_salv_form_layout_flow,
      go_alv_selections   TYPE REF TO cl_salv_selections,
      go_msg_ind          TYPE REF TO zcl_progress_indicator,
      gt_requests         TYPE trwbo_request_headers,
      gs_selection        TYPE trwbo_selection,
      gs_ranges           TYPE trsel_ts_ranges.

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
      alv_create_header_and_footer.


ENDCLASS.

DATA: go_request TYPE REF TO lcl_zrequest_report.

INITIALIZATION.

  CREATE OBJECT go_request.

CLASS lcl_zrequest_report IMPLEMENTATION.

  METHOD constructor.

    CALL METHOD me->alv_create.

    CALL METHOD trint_select_requests.

    CALL METHOD me->alv_show.

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


    gs_selection-reqfunctions       = 'CDEFGKLMOPTW'.
    gs_selection-reqstatus          = 'DLONR'.
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

      IF <fs_requests>-trstatus EQ co_trstatus-modifiable. "D  Modificável
        MOVE-CORRESPONDING:
            <fs_requests> TO ls_alv,
            ls_request-h  TO ls_alv.
*        ls_alv-client   = ls_request-h-client.
        ls_alv-systemid = sy-sysid.
        ls_alv-rc       = 99.

        CALL METHOD me->alv_set_tr_text
          CHANGING
            ch_s_alv = ls_alv.

        APPEND ls_alv TO me->gt_alv.

      ENDIF.

      LOOP AT ls_cofile-systems     ASSIGNING FIELD-SYMBOL(<fs_system>).
        LOOP AT <fs_system>-steps   ASSIGNING FIELD-SYMBOL(<fs_step>). "WHERE clientid NE space. " step of transport

          ls_alv-client   = ls_request-h-client.
          ls_alv-systemid = <fs_system>-systemid.
          ls_alv-rc       = <fs_system>-rc.

          LOOP AT <fs_step>-actions ASSIGNING FIELD-SYMBOL(<fs_action>). " action
            ls_alv-date = <fs_action>-date.
            ls_alv-time = <fs_action>-time.
          ENDLOOP.

          MOVE-CORRESPONDING:
            <fs_requests> TO ls_alv,
            ls_request-h  TO ls_alv.

          CALL METHOD me->alv_set_icon_type
            CHANGING
              ch_s_alv = ls_alv.

          CALL METHOD me->alv_set_tr_text
            CHANGING
              ch_s_alv = ls_alv.

          APPEND ls_alv TO me->gt_alv.

        ENDLOOP.
      ENDLOOP.
    ENDLOOP.

    DELETE ADJACENT DUPLICATES FROM me->gt_alv COMPARING trkorr systemid.

    CALL METHOD delete_system_not_final EXPORTING im_v_system = me->cc_production_sys.
    CALL METHOD delete_system_not_final EXPORTING im_v_system = me->cc_quality_sys.

    SORT me->gt_alv  DESCENDING BY date time .


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

    CLEAR: go_alv_flow_date, go_alv_flow_num_rec.

    go_alv_flow_date = go_alv_top_of_list->create_flow( row = 2 column  = 1 ).
    go_alv_flow_num_rec = go_alv_top_of_list->create_flow( row = 3 column  = 1 ).
    go_alv_flow_date->create_text(    text = |Data de execução: { sy-datlo DATE = USER } { syst-uzeit TIME = USER }| ).
    go_alv_flow_num_rec->create_text( text = 'Número total de registros selecionados: ' && |{ lines( me->gt_alv ) }| ).

    CALL METHOD me->go_alv_table->display.

    CALL METHOD me->user_command( syst-ucomm ).

  ENDMETHOD.

  METHOD on_alv_user_command.
    CALL METHOD me->user_command( e_salv_function ).
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
        CALL METHOD:
            me->trint_select_requests( im_v_refresh = abap_on ),
            me->go_alv_table->refresh.

      WHEN 'RDDIT076'.
        CALL METHOD me->call_rddit076( ).

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

  METHOD alv_create_header_and_footer .
    DATA:
*          lr_top_element TYPE REF TO cl_salv_form_layout_grid,
      lr_end_element TYPE REF TO cl_salv_form_layout_flow,
      lr_grid        TYPE REF TO cl_salv_form_layout_grid,
*          lr_header      TYPE REF TO cl_salv_form_header_info,
      lr_action      TYPE REF TO cl_salv_form_action_info,
      lr_textview1   TYPE REF TO cl_salv_form_text,
      lr_picture     TYPE REF TO cl_salv_form_picture,
      lr_label       TYPE REF TO cl_salv_form_label.

    TRY.
        " Header Top Of Page
*    lr_label = go_alv_top_of_list->create_label( row = 1 column  = 1 ).
*    lr_label->set_text( value =  ).

        CREATE OBJECT go_alv_top_of_list
          EXPORTING
            columns = 2.

        go_alv_top_of_list->create_header_information(
          EXPORTING
            row     = 1                 " Natural Number
            column  = 1                 " Natural Number
            text    = 'Relatório de Requests'(002)
            tooltip = 'Relatório de Requests'(002)   ).


        lr_grid = go_alv_top_of_list->create_grid( row    = 3
                                                   column = 1 ).


        lr_textview1 = lr_grid->create_text(
            row     = 1
            column  = 1
            text    = 'teste texto Row 1 Coluna 1'
            tooltip = 'Tooltip' ).


        CREATE OBJECT lr_picture
          EXPORTING
            picture_id = 'Z_SAPGUI_MC_001.PNG'.

        CALL METHOD lr_grid->set_element
          EXPORTING
            row       = 4
            column    = 1
            r_element = lr_picture.

        me->go_alv_table->set_top_of_list( value = go_alv_top_of_list ).

*    DATA: lr_eol TYPE REF TO cl_salv_form_header_info.
*    CREATE OBJECT lr_eol
*      EXPORTING
*        text = 'This is my Footer'.
*
*    me->go_alv_table->set_end_of_list( lr_eol ).

      CATCH cx_root INTO DATA(lcx_root).
        MESSAGE lcx_root->get_text( ) TYPE 'E'.
    ENDTRY.

  ENDMETHOD.
ENDCLASS.
