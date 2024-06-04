*&---------------------------------------------------------------------*
*& Report ZRCM0005_NORIF_EVERYDAY
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
REPORT ZRCM0005_NOTIF_EVERYDAY.
  DATA: lt_view_msg  TYPE ztt_mail_view_msg,
        lt_mail_send  TYPE ztt_mail_send,
        lt_par1_msg  TYPE ztt_mail_par1_msg,
        lt_plc_hld   TYPE ztt_mail_placeholders_data,
        lt_plc_hld_t TYPE ztt_mail_placeholders_data,
        lt_a005      TYPE TABLE OF ztrcm_a005,
        lr_a005_del  TYPE RANGE OF ztrcm_a005-case_guid,
        lr_stats     TYPE RANGE OF scmg_t_case_attr-stat_orderno,
        lr_stats_005 TYPE RANGE OF scmg_t_case_attr-stat_orderno,
        lr_stats_del TYPE RANGE OF scmg_t_case_attr-stat_orderno,
        lr_ext_key   TYPE RANGE OF scmg_t_case_attr-ext_key,
        lr_mails     TYPE RANGE OF ztmail_text-view_msg,
        ls_adr       TYPE bapiaddr3,
        lt_return    TYPE bapiret2_t,
        lv_sum       TYPE zrcmt_dgv_attr-sum_val,
        lv_comp_sum  TYPE zrcmt_dgv_attr-sum_val,
        lv_perc      TYPE p DECIMALS 2,
        lv_rem       TYPE i VALUE 0, "0 - нет значения, 1 - сумма, 2 - проценты
        lv_date      TYPE dats.

  lr_stats = VALUE #( ( sign = 'I' option = 'EQ' low = '20' )
                      ( sign = 'I' option = 'EQ' low = '30' )
                      ( sign = 'I' option = 'EQ' low = '40' )
                      ( sign = 'I' option = 'EQ' low = '70' ) ).

  lr_mails = VALUE #( ( sign = 'I' option = 'EQ' low = 'RCM_A006' )
                      ( sign = 'I' option = 'EQ' low = 'RCM_A003' )
                      ( sign = 'I' option = 'EQ' low = 'RCM_A004' )
                      ( sign = 'I' option = 'EQ' low = 'RCM_A005' )
                      ( sign = 'I' option = 'EQ' low = 'RCM_A008' ) ).

  lr_stats_005 = lr_stats.
  APPEND VALUE #( sign = 'I' option = 'EQ' low = '90' ) TO lr_stats_005.
  APPEND VALUE #( sign = 'I' option = 'EQ' low = '95' ) TO lr_stats_005.

  lr_stats_del = VALUE #( ( sign = 'I' option = 'EQ' low = '70' )
                          ( sign = 'I' option = 'EQ' low = '90' )
                          ( sign = 'I' option = 'EQ' low = '95' ) ).

  "Необходимые сообщения*
  SELECT *
    FROM ztmail_text
    INTO TABLE @DATA(lt_mail_data)
    WHERE view_msg IN @lr_mails.

  "RCM_A006
  SELECT SINGLE value
    FROM ztrcm_dgv_req
    INTO @DATA(lv_count_month)
    WHERE field_name = 'ENDMONTHS_NUM'.
  IF sy-subrc = 0 AND lv_count_month IS NOT INITIAL.
    CALL FUNCTION 'RP_CALC_DATE_IN_INTERVAL'
      EXPORTING
        date      = sy-datum
        days      = 0
        months    = CONV numc2( lv_count_month+0(2) )
        years     = 0
      IMPORTING
        calc_date = lv_date
      .
    IF lv_date IS NOT INITIAL.
      SELECT zattr~reg_num
           , zattr~date_okon
           , attr~responsible
           , attr~ext_key
        FROM zrcmt_dgv_attr AS zattr
        INNER JOIN scmg_t_case_attr AS attr ON attr~case_guid = zattr~case_guid
        WHERE zattr~date_okon = @lv_date
        INTO TABLE @DATA(lt_rcm_a006).
    ENDIF.

    READ TABLE lt_mail_data WITH KEY view_msg = 'RCM_A006' INTO DATA(ls_mail_data).
    IF sy-subrc = 0.
      LOOP AT lt_rcm_a006 INTO DATA(ls_rcm_a006).
        CLEAR lt_plc_hld.
        APPEND VALUE zsmail_placeholders_data( name_placeholders = '&1' data_placeholders = ls_rcm_a006-reg_num   ) TO lt_plc_hld.
        APPEND VALUE zsmail_placeholders_data(
          name_placeholders = '&2'
          data_placeholders = |{ ls_rcm_a006-date_okon+6(2) }.{ ls_rcm_a006-date_okon+4(2) }.{ ls_rcm_a006-date_okon+0(4) }| ) TO lt_plc_hld.
        APPEND VALUE zsmail_placeholders_data( name_placeholders = '&3' data_placeholders = lv_count_month        ) TO lt_plc_hld.
        APPEND VALUE zsmail_placeholders_data( name_placeholders = '&1' data_placeholders = ls_rcm_a006-reg_num   ) TO lt_plc_hld_t.

        CALL FUNCTION 'BAPI_USER_GET_DETAIL'
          EXPORTING
            username       = ls_rcm_a006-responsible
          IMPORTING
            address        = ls_adr
          TABLES
            return         = lt_return
        .
        IF ls_adr-e_mail IS NOT INITIAL.
          APPEND VALUE zsmail_send(
              email              = ls_adr-e_mail
              topic_msg          = ls_mail_data-topic_msg
              text_msg           = ls_mail_data-text_msg
              placeholders_body  = lt_plc_hld
              placeholders_topic = lt_plc_hld_t
          ) TO lt_mail_send.
        ENDIF.
      ENDLOOP.
    ENDIF.
  ENDIF.

  "RCM_A003
  SELECT SINGLE value
    FROM ztrcm_dgv_req
    INTO @DATA(lv_count_days)
    WHERE field_name = 'PAYDAYS_NUM'.
  IF sy-subrc = 0 AND lv_count_days IS NOT INITIAL.
    TRY.
        lv_date = sy-datum + conv i( lv_count_days ).

        SELECT dgv~reg_num
             , attr~responsible
          FROM scmg_t_case_attr AS attr
          INNER JOIN zrcmt_dgv_attr AS dgv ON dgv~case_guid = attr~case_guid
          INNER JOIN eban ON eban~zzext_key = attr~ext_key
          INNER JOIN ebkn ON ebkn~banfn = eban~banfn
                         AND ebkn~bnfpo = eban~bnfpo
                         AND ebkn~loekz = ''
          INTO TABLE @DATA(lt_data_a003)
          WHERE attr~stat_orderno IN @lr_stats
            AND eban~lfdat = @lv_date
            AND ebkn~zebkn = ( SELECT MAX( zebkn )
                                 FROM ebkn
                                 WHERE banfn = eban~banfn
                                   AND bnfpo = eban~bnfpo ).

        READ TABLE lt_mail_data WITH KEY view_msg = 'RCM_A003' INTO ls_mail_data.
        IF sy-subrc = 0 AND lt_data_a003 IS NOT INITIAL.
          SORT lt_data_a003 BY reg_num.
          LOOP AT lt_data_a003 INTO DATA(ls_a003) GROUP BY ls_a003-reg_num.
            CLEAR lt_plc_hld.
            APPEND VALUE zsmail_placeholders_data( name_placeholders = '&1' data_placeholders = ls_a003-reg_num   ) TO lt_plc_hld.
            APPEND VALUE zsmail_placeholders_data(
              name_placeholders = '&2'
              data_placeholders = |{ lv_date+6(2) }.{ lv_date+4(2) }.{ lv_date+0(4) }| ) TO lt_plc_hld.
            APPEND VALUE zsmail_placeholders_data( name_placeholders = '&3' data_placeholders = lv_count_days     ) TO lt_plc_hld.

            CALL FUNCTION 'BAPI_USER_GET_DETAIL'
              EXPORTING
                username       = ls_a003-responsible
              IMPORTING
                address        = ls_adr
              TABLES
                return         = lt_return
            .
            IF ls_adr-e_mail IS NOT INITIAL.
              APPEND VALUE zsmail_send(
                  email              = ls_adr-e_mail
                  topic_msg          = ls_mail_data-topic_msg
                  text_msg           = ls_mail_data-text_msg
                  placeholders_body  = lt_plc_hld
              ) TO lt_mail_send.
            ENDIF.
          ENDLOOP.
        ENDIF.
      CATCH cx_sy_conversion_no_number.
    ENDTRY.
  ENDIF.

  "RCM_A008
  SELECT SINGLE value
    FROM ztrcm_dgv_req
    INTO lv_count_days
    WHERE field_name = 'PAYDAYS_NUM'.
  IF sy-subrc = 0 AND lv_count_days IS NOT INITIAL.
    TRY.
        lv_date = sy-datum + conv i( lv_count_days ).

        SELECT attr~responsible
             , dgv~reg_num
          FROM scmg_t_case_attr AS attr
          INNER JOIN zrcmt_dgv_attr AS dgv ON dgv~case_guid = attr~case_guid
          INNER JOIN kblk ON kblk~zzdog = attr~ext_key
          INNER JOIN kblp AS p1 ON p1~belnr = kblk~belnr
                               AND p1~loekz = ''
          WHERE ( p1~zztype_pay = 'A' OR p1~zztype_pay = 'P' )
            AND kblk~bltyp = '040'
            AND p1~fdatk = @lv_date
            AND attr~stat_orderno IN @lr_stats
          INTO TABLE @DATA(lt_a008).

        READ TABLE lt_mail_data WITH KEY view_msg = 'RCM_A008' INTO ls_mail_data.
        IF sy-subrc = 0 AND lt_a008 IS NOT INITIAL.
          SORT lt_data_a003 BY reg_num.
          LOOP AT lt_a008 INTO DATA(ls_a008) GROUP BY ls_a008-reg_num.
            CLEAR lt_plc_hld.
            APPEND VALUE zsmail_placeholders_data( name_placeholders = '&1' data_placeholders = ls_a008-reg_num   ) TO lt_plc_hld.
            APPEND VALUE zsmail_placeholders_data(
              name_placeholders = '&2'
              data_placeholders = |{ lv_date+6(2) }.{ lv_date+4(2) }.{ lv_date+0(4) }| ) TO lt_plc_hld.
            APPEND VALUE zsmail_placeholders_data( name_placeholders = '&3' data_placeholders = lv_count_days     ) TO lt_plc_hld.

            CALL FUNCTION 'BAPI_USER_GET_DETAIL'
              EXPORTING
                username       = ls_a008-responsible
              IMPORTING
                address        = ls_adr
              TABLES
                return         = lt_return
            .
            IF ls_adr-e_mail IS NOT INITIAL.
              APPEND VALUE zsmail_send(
                  email              = ls_adr-e_mail
                  topic_msg          = ls_mail_data-topic_msg
                  text_msg           = ls_mail_data-text_msg
                  placeholders_body  = lt_plc_hld
              ) TO lt_mail_send.
            ENDIF.
          ENDLOOP.
        ENDIF.
      CATCH cx_sy_conversion_no_number.
    ENDTRY.
  ENDIF.

  "RCM_A004
  SELECT SINGLE value
    FROM ztrcm_dgv_req
    INTO lv_count_days
    WHERE field_name = 'DELIVDAYS_NUM'.
  IF sy-subrc = 0 AND lv_count_days IS NOT INITIAL.
    TRY.
        lv_date = sy-datum + conv i( lv_count_days ).

        "MM
        SELECT attr~responsible
             , dgv_attr~reg_num
          FROM ztrcm_079_dvg_mm AS dgv
          INNER JOIN scmg_t_case_attr AS attr ON attr~case_guid = dgv~case_guid
          INNER JOIN zrcmt_dgv_attr AS dgv_attr ON dgv_attr~case_guid = attr~case_guid
          INNER JOIN ekpo AS p ON p~ebeln = dgv~ebeln
                              AND p~ebelp = dgv~ebelp
           LEFT JOIN ekkn AS n ON p~ebeln = n~ebeln
                              AND p~ebelp = n~ebelp
          INNER JOIN ekko AS h ON h~ebeln = p~ebeln

          INTO TABLE @DATA(lt_a004_mm)
          WHERE attr~stat_orderno IN @lr_stats
            AND p~audat = @lv_date
          GROUP BY attr~responsible, dgv_attr~reg_num.

        "SD
        SELECT attr~responsible
             , dgv_attr~reg_num
        FROM ztrcm_079_dvg_sd AS dgv
        INNER JOIN scmg_t_case_attr AS attr ON attr~case_guid = dgv~case_guid
        INNER JOIN zrcmt_dgv_attr AS dgv_attr ON dgv_attr~case_guid = attr~case_guid
        INNER JOIN vbap AS p ON p~vbeln = dgv~vbeln
                            AND p~posnr = dgv~posnr
        INNER JOIN vbak AS h ON h~vbeln = p~vbeln
        INTO TABLE @DATA(lt_a004_sd)
        WHERE attr~stat_orderno IN @lr_stats
          AND h~vdatu = @lv_date
        GROUP BY attr~responsible, dgv_attr~reg_num.

        "RE
        SELECT attr~responsible
             , dgv_attr~reg_num
        FROM ztrcm_079_dvg_re AS dgv
        INNER JOIN scmg_t_case_attr AS attr ON attr~case_guid = dgv~case_guid
        INNER JOIN zrcmt_dgv_attr AS dgv_attr ON dgv_attr~case_guid = attr~case_guid
        INNER JOIN vicncn AS h ON h~recnnr = dgv~recnnr
                              AND h~zzrcmnum = dgv~zzrcmnum
        LEFT JOIN vicdcond AS p ON p~intreno = h~intreno
        INTO TABLE @DATA(lt_a004_re)
        WHERE attr~stat_orderno IN @lr_stats
          AND h~recnnotreceipt = @lv_date
        GROUP BY attr~responsible, dgv_attr~reg_num.

        READ TABLE lt_mail_data WITH KEY view_msg = 'RCM_A004' INTO ls_mail_data.
        IF sy-subrc = 0.

          LOOP AT lt_a004_mm INTO DATA(ls_a004_mm).
            CLEAR lt_plc_hld.
            APPEND VALUE zsmail_placeholders_data( name_placeholders = '&1' data_placeholders = ls_a004_mm-reg_num   ) TO lt_plc_hld.
            APPEND VALUE zsmail_placeholders_data(
              name_placeholders = '&2'
              data_placeholders = |{ lv_date+6(2) }.{ lv_date+4(2) }.{ lv_date+0(4) }| ) TO lt_plc_hld.
            APPEND VALUE zsmail_placeholders_data( name_placeholders = '&3' data_placeholders = lv_count_days     ) TO lt_plc_hld.

            CALL FUNCTION 'BAPI_USER_GET_DETAIL'
              EXPORTING
                username       = ls_a004_mm-responsible
              IMPORTING
                address        = ls_adr
              TABLES
                return         = lt_return
            .
            IF ls_adr-e_mail IS NOT INITIAL.
              APPEND VALUE zsmail_send(
                  email              = ls_adr-e_mail
                  topic_msg          = ls_mail_data-topic_msg
                  text_msg           = ls_mail_data-text_msg
                  placeholders_body  = lt_plc_hld
              ) TO lt_mail_send.
            ENDIF.
          ENDLOOP.


          LOOP AT lt_a004_sd INTO DATA(ls_a004_sd).
            CLEAR lt_plc_hld.
            APPEND VALUE zsmail_placeholders_data( name_placeholders = '&1' data_placeholders = ls_a004_sd-reg_num   ) TO lt_plc_hld.
            APPEND VALUE zsmail_placeholders_data(
              name_placeholders = '&2'
              data_placeholders = |{ lv_date+6(2) }.{ lv_date+4(2) }.{ lv_date+0(4) }| ) TO lt_plc_hld.
            APPEND VALUE zsmail_placeholders_data( name_placeholders = '&3' data_placeholders = lv_count_days     ) TO lt_plc_hld.

            CALL FUNCTION 'BAPI_USER_GET_DETAIL'
              EXPORTING
                username       = ls_a004_sd-responsible
              IMPORTING
                address        = ls_adr
              TABLES
                return         = lt_return
            .
            IF ls_adr-e_mail IS NOT INITIAL.
              APPEND VALUE zsmail_send(
                  email              = ls_adr-e_mail
                  topic_msg          = ls_mail_data-topic_msg
                  text_msg           = ls_mail_data-text_msg
                  placeholders_body  = lt_plc_hld
              ) TO lt_mail_send.
            ENDIF.
          ENDLOOP.

          LOOP AT lt_a004_re INTO DATA(ls_a004_re).
            CLEAR lt_plc_hld.
            APPEND VALUE zsmail_placeholders_data( name_placeholders = '&1' data_placeholders = ls_a004_re-reg_num   ) TO lt_plc_hld.
            APPEND VALUE zsmail_placeholders_data(
              name_placeholders = '&2'
              data_placeholders = |{ lv_date+6(2) }.{ lv_date+4(2) }.{ lv_date+0(4) }| ) TO lt_plc_hld.
            APPEND VALUE zsmail_placeholders_data( name_placeholders = '&3' data_placeholders = lv_count_days     ) TO lt_plc_hld.

            CALL FUNCTION 'BAPI_USER_GET_DETAIL'
              EXPORTING
                username       = ls_a004_re-responsible
              IMPORTING
                address        = ls_adr
              TABLES
                return         = lt_return
            .
            IF ls_adr-e_mail IS NOT INITIAL.
              APPEND VALUE zsmail_send(
                  email              = ls_adr-e_mail
                  topic_msg          = ls_mail_data-topic_msg
                  text_msg           = ls_mail_data-text_msg
                  placeholders_body  = lt_plc_hld
              ) TO lt_mail_send.
            ENDIF.
          ENDLOOP.

        ENDIF.
      CATCH cx_sy_conversion_no_number.
    ENDTRY.
  ENDIF.

  "RCM_A005
  SELECT field_name, value
    FROM ztrcm_dgv_req
    INTO TABLE @DATA(lt_dgv_req)
    WHERE field_name = 'REM_SUM'
       OR field_name = 'REM_PERC'
       OR field_name = 'REM_TYPE'.
  IF sy-subrc = 0.
    TRY.
        READ TABLE lt_dgv_req WITH KEY field_name = 'REM_TYPE' INTO DATA(ls_dgv_req).
        IF sy-subrc = 0 AND ls_dgv_req-value = '1'.
          READ TABLE lt_dgv_req WITH KEY field_name = 'REM_SUM' INTO ls_dgv_req.
          IF sy-subrc = 0 .
            lv_sum = ls_dgv_req-value.
            lv_rem = 1.
          ENDIF.
        ELSEIF sy-subrc = 0 AND ls_dgv_req-value = '0'.
          READ TABLE lt_dgv_req WITH KEY field_name = 'REM_PERC' INTO ls_dgv_req.
          IF sy-subrc = 0 .
            lv_perc = ls_dgv_req-value.
            lv_rem = 2.
          ENDIF.
        ENDIF.



      CATCH cx_sy_conversion_no_number.
    ENDTRY.

    READ TABLE lt_mail_data WITH KEY view_msg = 'RCM_A005' INTO ls_mail_data.
    IF sy-subrc = 0.
      SELECT dgv~reg_num
           , dgv~sum_val
           , dgv~valuta
           , attr~responsible
           , attr~ext_key
           , attr~stat_orderno
           , a005~case_guid
           , a005~rem_sum
        FROM zrcmt_dgv_attr AS dgv
        INNER JOIN scmg_t_case_attr AS attr ON attr~case_guid = dgv~case_guid
        LEFT JOIN ztrcm_a005 AS a005 ON a005~case_guid = dgv~case_guid
        INTO TABLE @DATA(lt_a005_h)
        WHERE attr~stat_orderno IN @lr_stats_005
          AND attr~case_type LIKE 'DGV%'.


      lr_a005_del = VALUE #( FOR ls_del IN lt_a005_h WHERE ( stat_orderno IN lr_stats_del )
                                 ( sign = 'I' option = 'EQ' low = ls_del-case_guid ) ).
      DELETE lt_a005_h WHERE stat_orderno IN lr_stats_del.

      LOOP AT lt_a005_h ASSIGNING FIELD-SYMBOL(<ls_a005_h>).
        <ls_a005_h>-ext_key = |{ <ls_a005_h>-ext_key ALPHA = IN }|.
      ENDLOOP.

      lr_ext_key = VALUE #( FOR ls_ext_key IN lt_a005_h ( sign = 'I'
                                                          option = 'EQ'
                                                          low = ls_ext_key-ext_key ) ).

      IF lr_ext_key IS NOT INITIAL.
        SELECT acdoca~zuonr
             , sum( CASE WHEN acdoca~wsl IS NOT INITIAL THEN acdoca~wsl
                    ELSE acdoca~tsl END ) AS sum
          FROM scmg_t_case_attr AS attr
          INNER JOIN acdoca ON acdoca~rldnr = '0L'
                           AND ( acdoca~blart = 'KZ' OR acdoca~blart = 'KA' )
                           AND acdoca~xreversing = ''
                           AND acdoca~xreversed = ''
                           AND acdoca~koart = 'K'
          WHERE acdoca~zuonr IN @lr_ext_key
          GROUP BY acdoca~zuonr
          INTO TABLE @DATA(lt_a005_p).

        LOOP AT lt_a005_h ASSIGNING <ls_a005_h>.
          CLEAR: lv_comp_sum. "Сумма сравнения
          IF lv_rem = 1.
            lv_comp_sum = lv_sum.
          ELSEIF lv_rem = 2.
            lv_comp_sum = <ls_a005_h>-sum_val * lv_perc / 100.
          ELSE.
            EXIT.
          ENDIF.
          READ TABLE lt_a005_p WITH KEY zuonr = <ls_a005_h>-ext_key INTO DATA(ls_a005_p).
          IF sy-subrc = 0 AND lv_comp_sum > ls_a005_p-sum AND ( ( <ls_a005_h>-case_guid IS NOT INITIAL AND <ls_a005_h>-rem_sum <> lv_comp_sum )
                                                                 OR <ls_a005_h>-case_guid IS INITIAL ).
            CLEAR: lt_plc_hld, lt_plc_hld_t.
            APPEND VALUE zsmail_placeholders_data( name_placeholders = '&1' data_placeholders = <ls_a005_h>-reg_num   ) TO lt_plc_hld_t.
            APPEND VALUE zsmail_placeholders_data( name_placeholders = '&1' data_placeholders = <ls_a005_h>-reg_num   ) TO lt_plc_hld.
            IF lv_rem = 1.
              APPEND VALUE zsmail_placeholders_data( name_placeholders = '&2' data_placeholders = lv_sum ) TO lt_plc_hld.
              APPEND VALUE zsmail_placeholders_data( name_placeholders = '&3' data_placeholders = <ls_a005_h>-valuta ) TO lt_plc_hld.
            ELSE.
              APPEND VALUE zsmail_placeholders_data( name_placeholders = '&2' data_placeholders = lv_perc ) TO lt_plc_hld.
              APPEND VALUE zsmail_placeholders_data( name_placeholders = '&3' data_placeholders = '%' ) TO lt_plc_hld.
            ENDIF.

            CALL FUNCTION 'BAPI_USER_GET_DETAIL'
              EXPORTING
                username       = ls_a004_sd-responsible
              IMPORTING
                address        = ls_adr
              TABLES
                return         = lt_return
            .
            IF ls_adr-e_mail IS NOT INITIAL.
              APPEND VALUE ztrcm_a005(
                  case_guid = <ls_a005_h>-case_guid
                  rem_sum   = lv_comp_sum
              ) TO lt_a005.
              APPEND VALUE zsmail_send(
                  email              = ls_adr-e_mail
                  topic_msg          = ls_mail_data-topic_msg
                  text_msg           = ls_mail_data-text_msg
                  placeholders_body  = lt_plc_hld
                  placeholders_topic = lt_plc_hld_t
              ) TO lt_mail_send.
            ENDIF.
          ELSEIF sy-subrc = 0 AND <ls_a005_h>-case_guid IS NOT INITIAL AND lv_comp_sum < ls_a005_p-sum.
            APPEND VALUE #( sign = 'I' option = 'EQ' low = <ls_a005_h>-case_guid ) TO lr_a005_del.
          ENDIF.
        ENDLOOP.
      ENDIF.
    ENDIF.
  ENDIF.

  IF lt_mail_send IS NOT INITIAL.
    TRY.
        CALL FUNCTION 'Z_SEND_EMAIL'                " Таблица доп. параметра 3
          TABLES
            it_mail_send         = lt_mail_send                 " Таблица отправки уведомления
          .
        IF lt_a005 IS NOT INITIAL.
          MODIFY ztrcm_a005 FROM TABLE lt_a005.
        ENDIF.
        IF lr_a005_del IS NOT INITIAL.
          DELETE FROM ztrcm_a005 WHERE case_guid IN lr_a005_del.
        ENDIF.
        COMMIT WORK AND WAIT.
      CATCH cx_bcs_send. " Exceptions Send Interface
    ENDTRY.
  ENDIF.
