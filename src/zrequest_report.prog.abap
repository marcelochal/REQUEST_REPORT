REPORT zrequest_report.

TABLES: tmsbufreq,
        tstrfcofil,
        e070,
        e070c,
        e07t,
        e070ctv.

TYPE-POOLS: ctslg, slis, icon, trwbo, trsel.

INCLUDE: rddkorri.

TYPES:

  BEGIN OF ty_alv,
    trkorr       TYPE trkorr,
    trfunction   LIKE e070-trfunction,
    trfunctn_t   LIKE ko013-trfunctn_t,
    systemid     TYPE tarsystem,  " target system
*    clientid     TYPE mandt,     " target client (mandt)
    rc           TYPE strw_int4,  " return code
    rc_icon      TYPE tp_icon,    " return code
    trstatus     TYPE trstatus,
    trstatus_txt TYPE trstatus_t,
    date         TYPE as4date,
    time         TYPE as4time,
    as4user      TYPE tr_as4user,
    as4text      TYPE as4text,

  END OF ty_alv.

CONSTANTS:
  cc_development_sys TYPE tarsystem VALUE 'S4D',
  cc_quality_sys     TYPE tarsystem VALUE 'S4Q',
  cc_production_sys  TYPE tarsystem VALUE 'S4P'.

DATA:
  it_alv       TYPE STANDARD TABLE OF ty_alv WITH NON-UNIQUE SORTED KEY key1 COMPONENTS trkorr,
  it_requests  TYPE trwbo_request_headers,
  it_events    TYPE STANDARD TABLE OF slis_alv_event,
  is_selection TYPE trwbo_selection,
  cs_ranges    TYPE trsel_ts_ranges.


INITIALIZATION.
  PERFORM strh_initialization
    CHANGING is_selection-trkorrpattern.
  PERFORM trint_select_requests.

AT USER-COMMAND.
  CASE sy-ucomm.
    WHEN '%EX'.
      LEAVE TO LIST-PROCESSING.
    WHEN '&F03'.
      PERFORM trint_select_requests.

  ENDCASE.


**********************************************************************
START-OF-SELECTION.


*&---------------------------------------------------------------------*
*& Form  trint_select_requests
*&---------------------------------------------------------------------*
FORM trint_select_requests.

*K   Ordem de workbench
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


  CLEAR: it_requests, it_alv.

  CALL FUNCTION 'TRINT_SELECT_REQUESTS'
    EXPORTING
      iv_username_pattern    = sy-uname
      is_selection           = is_selection
      iv_complete_projects   = 'X'
      iv_via_selscreen       = abap_true
      iv_title               = 'Lista de status de requests'(001)
    IMPORTING
      et_requests            = it_requests
    CHANGING
      cs_ranges              = cs_ranges
    EXCEPTIONS
      action_aborted_by_user = 1
      OTHERS                 = 2.

  IF sy-subrc EQ 1.
    LEAVE PROGRAM.
  ELSEIF sy-subrc NE 0.
    MESSAGE ID sy-msgid TYPE 'S' NUMBER sy-msgno DISPLAY LIKE 'S'
               WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
    RETURN.
  ENDIF.

  PERFORM data_retrieval.
*  PERFORM text USING pg_head.
  PERFORM alv_display_report.

ENDFORM.

**********************************************************************
* Form
**********************************************************************
FORM data_retrieval .

  DATA:
    es_cofile TYPE ctslg_cofile,
    ls_alv    TYPE ty_alv.

  FIELD-SYMBOLS:
    <fs_requests> LIKE LINE OF it_requests,
    <fs_system>   TYPE ctslg_system,
    <fs_step>     TYPE ctslg_step,
    <fs_action>   TYPE ctslg_action.


  LOOP AT it_requests ASSIGNING <fs_requests> WHERE trfunction IN cs_ranges-request_funcs.

    CLEAR ls_alv.

    CALL FUNCTION 'TR_READ_GLOBAL_INFO_OF_REQUEST'
      EXPORTING
        iv_trkorr = <fs_requests>-trkorr
      IMPORTING
        es_cofile = es_cofile.

    IF <fs_requests>-trstatus EQ 'D'. "D  Modificável
      MOVE-CORRESPONDING <fs_requests> TO ls_alv.
      ls_alv-systemid = sy-sysid.
      ls_alv-rc       = 99.
      ls_alv-trkorr   = <fs_requests>-trkorr.
      ls_alv-as4user  = <fs_requests>-as4user.
      ls_alv-trstatus = <fs_requests>-trstatus.

      APPEND ls_alv TO it_alv.

    ENDIF.

    LOOP AT es_cofile-systems ASSIGNING <fs_system>.

      LOOP AT <fs_system>-steps ASSIGNING <fs_step> WHERE clientid NE space. " step of transport

        CLEAR ls_alv.

        ls_alv-systemid = <fs_system>-systemid.
*      wa_alv-clientid = wa_system-systemid.
        ls_alv-rc = <fs_system>-rc.
*        wa_alv-clientid = wa_step-clientid.
*        wa_alv-rc = wa_step-rc.

        LOOP AT <fs_step>-actions ASSIGNING <fs_action>. " action
          ls_alv-date = <fs_action>-date.
          ls_alv-time = <fs_action>-time.
        ENDLOOP.

        MOVE-CORRESPONDING <fs_requests> TO ls_alv.
*        wa_alv-trkorr   = wa_request-trkorr.
*        wa_alv-as4user  = wa_request-as4user.
*        wa_alv-trstatus = wa_request-trstatus.

        APPEND ls_alv TO it_alv.

      ENDLOOP.
    ENDLOOP.
  ENDLOOP.

*  DELETE ADJACENT DUPLICATES FROM it_alv COMPARING clientid.

  PERFORM f_delete_system_not_final USING cc_production_sys.
  PERFORM f_delete_system_not_final USING cc_quality_sys.


ENDFORM. "DATA_RETRIEVAL

*&---------------------------------------------------------------------*
*& Form  alv_create_fieldcatalog
*&---------------------------------------------------------------------*
* Cria catalogo de campos a partir de uma tabela interna
*----------------------------------------------------------------------*
*      -->PT_TABLE     Tabela Interna
*      -->PT_FIELDCAT  Catalogo de campos
*----------------------------------------------------------------------*
FORM  alv_create_fieldcatalog
       USING     p_structure  TYPE any
       CHANGING  pt_fieldcat  TYPE slis_t_fieldcat_alv.

  DATA:
    lr_tabdescr TYPE REF TO cl_abap_structdescr,
    lr_data     TYPE REF TO data,
    lt_dfies    TYPE ddfields.

  FIELD-SYMBOLS:
    <fs_fieldcat> LIKE LINE OF pt_fieldcat.

  CLEAR:
    pt_fieldcat.

  CREATE DATA lr_data LIKE p_structure.

  lr_tabdescr ?= cl_abap_structdescr=>describe_by_data_ref( lr_data ).

  lt_dfies = cl_salv_data_descr=>read_structdescr( lr_tabdescr ).

  MOVE-CORRESPONDING lt_dfies TO pt_fieldcat.

  LOOP AT pt_fieldcat ASSIGNING <fs_fieldcat>.

*  Define os campos como centralizado
    <fs_fieldcat>-just = 'C'.

    CASE <fs_fieldcat>-fieldname.

      WHEN 'TRKORR'.
        <fs_fieldcat>-hotspot = abap_true.

      WHEN 'TRFUNCTION' OR 'RC' OR 'TRSTATUS'.
        <fs_fieldcat>-tech = abap_true.

      WHEN 'TRFUNCTN_T'.

        <fs_fieldcat>-rollname = 'TRFUNCTION'.

      WHEN 'RC_ICON'. "Icone return code

        <fs_fieldcat>-seltext_l = 'Log Erro'.
        <fs_fieldcat>-seltext_m = 'Log Erro'.
        <fs_fieldcat>-seltext_s = 'Log Erro'.

      WHEN 'TRSTATUS_TXT'.

        <fs_fieldcat>-rollname = 'TRSTATUS'.
        <fs_fieldcat>-ref_fieldname = <fs_fieldcat>-fieldname.
        <fs_fieldcat>-ref_tabname = 'E070'.
        <fs_fieldcat>-outputlen = 10.

      WHEN 'AS4USER'.
        <fs_fieldcat>-outputlen = 15.

      WHEN 'AS4TEXT'.
        <fs_fieldcat>-just = 'L'.

    ENDCASE.

  ENDLOOP.

ENDFORM.                    "alv_create_fieldcatalog

**********************************************************************
FORM alv_display_report.

  DATA:
    it_fieldcat            TYPE slis_t_fieldcat_alv, "IT FIELDCAT
    ls_layout              TYPE slis_layout_alv,
    es_exit_caused_by_user TYPE slis_exit_by_user,
    ls_alv                 TYPE ty_alv.

  FIELD-SYMBOLS:
    <fs_alv> LIKE LINE OF it_alv.

*  PERFORM getevents.
  PERFORM alv_create_fieldcatalog USING ls_alv CHANGING it_fieldcat.
  PERFORM alv_top_of_page.

  ls_layout-box_tabname = 'IT'.
  ls_layout-info_fieldname = 'LINE_COLOR'.
  ls_layout-zebra = abap_true.
  ls_layout-cell_merge = abap_true.

  LOOP AT it_alv ASSIGNING <fs_alv>.


    CASE <fs_alv>-rc.
      WHEN 0.                               "Sucess
        <fs_alv>-rc_icon = icon_led_green.
      WHEN 4 OR 1204.                       "Warning
        <fs_alv>-rc_icon = icon_led_yellow.
      WHEN 8.                               "Error
        <fs_alv>-rc_icon = icon_led_red.
      WHEN 12 OR 13 OR 14 OR 16.            "Cancel
        <fs_alv>-rc_icon = icon_message_critical.
      WHEN 99.
        <fs_alv>-rc_icon = icon_space.
    ENDCASE.


    CALL FUNCTION 'TR_REQUEST_TYPE_TEXT'
      EXPORTING
        iv_request_type        = <fs_alv>-trfunction
        iv_request_status      = <fs_alv>-trstatus
      IMPORTING
        ev_request_type_text   = <fs_alv>-trfunctn_t
*       ev_request_type_text_long =
        ev_request_status_text = <fs_alv>-trstatus_txt.

  ENDLOOP.

  CALL FUNCTION 'REUSE_ALV_GRID_DISPLAY'
    EXPORTING
      i_callback_program       = syst-cprog
      i_callback_top_of_page   = 'ALV_TOP_OF_PAGE'
      i_callback_pf_status_set = 'SET_ALV_STATUS'
      i_callback_user_command  = 'USER_COMMAND'
      is_layout                = ls_layout
      it_fieldcat              = it_fieldcat
      it_events                = it_events
*     it_sort                  =
      i_default                = 'X'
    TABLES
      t_outtab                 = it_alv
    EXCEPTIONS
      program_error            = 1
      OTHERS                   = 2.

  IF sy-subrc NE 0.
    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
    WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
  ELSE.
    SET USER-COMMAND '&F03'.
  ENDIF.

ENDFORM. "display_alv_report.
*&---------------------------------------------------------------------*
*&      Form  F_DELETE_SYSTEM_NOT_FINAL
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_CC_PRODUCTION_SYS  text
*      <--P_IT_ALV  text
*----------------------------------------------------------------------*
FORM f_delete_system_not_final USING  p_sys .

  DATA: lt_alv TYPE TABLE OF ty_alv,
        ls_alv TYPE ty_alv.

  MOVE it_alv TO lt_alv.

  LOOP AT lt_alv INTO ls_alv WHERE systemid = p_sys.
    DELETE it_alv WHERE trkorr = ls_alv-trkorr AND systemid NE ls_alv-systemid.
  ENDLOOP.

  DELETE ADJACENT DUPLICATES FROM it_alv COMPARING trkorr systemid.

ENDFORM.

*&---------------------------------------------------------------------*
*& Form USER_COMMAND
*&---------------------------------------------------------------------*
*& Form utilizado dinamicamente pela ALV
*&---------------------------------------------------------------------*
*& -->  p_ucomm        parametro com o user command
*& <--  p_sel          Dados da alv
*&---------------------------------------------------------------------*
FORM user_command USING p_ucomm LIKE sy-ucomm
                        p_sel   TYPE slis_selfield .

  CONSTANTS:
    cc_double_click(4)  TYPE c VALUE '&IC1'.

  DATA:
    ls_alv         TYPE ty_alv,
    lv_transaction TYPE tb_rfha.

  CASE p_ucomm.
    WHEN cc_double_click.

      IF p_sel-fieldname EQ 'TRKORR'
        AND p_sel-value IS NOT INITIAL.

        MOVE p_sel-value TO ls_alv-trkorr.

        CALL FUNCTION 'TR_PRESENT_REQUEST'
          EXPORTING
            iv_trkorr = ls_alv-trkorr.

      ENDIF.

      IF p_sel-fieldname EQ 'RC_ICON'.

        READ TABLE it_alv INDEX p_sel-tabindex TRANSPORTING trkorr INTO ls_alv.

        CALL FUNCTION 'TR_LOG_OVERVIEW_REQUEST_REMOTE'
          EXPORTING
            iv_trkorr = ls_alv-trkorr.

      ENDIF.

  ENDCASE.

ENDFORM.

*-------------------------------------------------------------------*
* Form  alv_top_of_page                                               *
*-------------------------------------------------------------------*
* ALV Report Header                                                 *
*-------------------------------------------------------------------*
FORM alv_top_of_page.
*ALV Header declarations
  DATA: t_header  TYPE slis_t_listheader,
        wa_header TYPE slis_listheader,
        t_line    LIKE wa_header-info,
        ld_lines  TYPE string.

* Title
  wa_header-typ  = 'H'.
  wa_header-info = 'Relatório de Requests'(002).
  APPEND wa_header TO t_header.
  CLEAR wa_header.

* Date
  wa_header-typ  = 'S'.
  wa_header-key = 'Data: '.
  CONCATENATE  sy-datum+6(2) '.'
               sy-datum+4(2) '.'
               sy-datum(4) INTO wa_header-info.   "todays date
  APPEND wa_header TO t_header.
  CLEAR: wa_header.

  wa_header-typ  = 'S'.
  wa_header-info = 'Número total de registros selecionados: ' && |{ lines( it_alv ) }|.
  APPEND wa_header TO t_header.

  CALL FUNCTION 'REUSE_ALV_COMMENTARY_WRITE'
    EXPORTING
      it_list_commentary = t_header
*     i_logo             =
      i_end_of_list_grid = abap_false
*     i_alv_form         =
    .

ENDFORM.                    "top-of-page

*&---------------------------------------------------------------------*
*&      Form  GETEVENTS
*&---------------------------------------------------------------------*
*      -->PT_EVENTS  text
*----------------------------------------------------------------------*
FORM getevents.

  DATA:
    wa_events TYPE slis_alv_event.

  CALL FUNCTION 'REUSE_ALV_EVENTS_GET'
    EXPORTING
      i_list_type     = 0
    IMPORTING
      et_events       = it_events
    EXCEPTIONS
      list_type_wrong = 1
      OTHERS          = 2.
  IF sy-subrc <> 0.
* MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
*         WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
  ENDIF.

  READ TABLE it_events INTO wa_events WITH KEY name = 'TOP_OF_PAGE'.
  wa_events-form = 'TOP_OF_PAGE'.

  MODIFY it_events FROM wa_events TRANSPORTING form WHERE name = wa_events-name.

ENDFORM.                    " GETEVENTS

*&---------------------------------------------------------------------*
*&      Form  SET_CELL_COLOURS
*&---------------------------------------------------------------------*
*       Set colour of individual ALV cell, field
*----------------------------------------------------------------------*
FORM set_cell_colours .
  DATA: wa_cellcolor TYPE lvc_s_scol,
        ld_index     TYPE sy-tabix,
        wa_alv       TYPE ty_alv.

  LOOP AT it_alv INTO wa_alv.

    ld_index = sy-tabix.

*   Set colour of EBELN field to various colors based on sy-tabix value
    wa_cellcolor-fname = 'EBELN'.
    wa_cellcolor-color-col = sy-tabix.  "color code 1-7, if outside rage defaults to 7
    wa_cellcolor-color-int = '1'.  "1 = Intensified on, 0 = Intensified off
    wa_cellcolor-color-inv = '0'.  "1 = text colour, 0 = background colour
*    APPEND wa_cellcolor TO wa_alv-cellcolor.
*    MODIFY it_ekko FROM wa_ekko INDEX ld_index TRANSPORTING cellcolor.

*   Set colour of NETPR field to color 4 if gt 0
    wa_cellcolor-fname = 'NETPR'.
    wa_cellcolor-color-col = 4.  "color code 1-7, if outside rage defaults to 7
    wa_cellcolor-color-int = '0'.  "1 = Intensified on, 0 = Intensified off
    wa_cellcolor-color-inv = '0'.  "1 = text colour, 0 = background colour
*      APPEND wa_cellcolor TO wa_ekko-cellcolor.
*      MODIFY it_ekko FROM wa_ekko INDEX ld_index TRANSPORTING cellcolor.

*   Set colour of AEDAT field text to red(6)
    wa_cellcolor-fname = 'AEDAT'.
    wa_cellcolor-color-col = 6.  "color code 1-7, if outside rage defaults to 7
    wa_cellcolor-color-int = '0'.  "1 = Intensified on, 0 = Intensified off
    wa_cellcolor-color-inv = '1'.  "1 = text colour, 0 = background colour
*    APPEND wa_cellcolor TO wa_ekko-cellcolor.
*    MODIFY it_ekko FROM wa_ekko INDEX ld_index TRANSPORTING cellcolor.
  ENDLOOP.

ENDFORM.                    " SET_CELL_COLOURS
*&---------------------------------------------------------------------*
*&      Form  STRH_INITIALIZATION
*&---------------------------------------------------------------------*
FORM strh_initialization CHANGING p_sysk_pattern LIKE e070-trkorr.

  DATA: lv_sys_pattern  LIKE e070-trkorr.

  is_selection-reqfunctions  = 'CDEFGKLMOPTW'.
  is_selection-reqstatus     = 'DLONR'.

  CALL FUNCTION 'TR_SYS_PARAMS'
    IMPORTING
      systemname = system_name
      systemtype = system_type.

  lv_sys_pattern(3)     = system_name(3).
  lv_sys_pattern+3(1)   = '%'.
  p_sysk_pattern(3)     = system_name(3).
  p_sysk_pattern+3(2)   = 'K*'.

ENDFORM.                               " STRH_INITIALIZATION
*&---------------------------------------------------------------------*
*& Form SET_ALV_STATUS
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM set_alv_status USING p_extab TYPE kkblo_t_extab.
  SET PF-STATUS 'ALV_STATUS'.

ENDFORM.
