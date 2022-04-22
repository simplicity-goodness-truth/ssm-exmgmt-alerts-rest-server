class ZCL_ALERT_ACTIONS_HANDLER definition
  public
  final
  create public .

public section.

  interfaces IF_HTTP_EXTENSION .
protected section.
private section.

  class-methods IS_ALERT_EXIST
    importing
      !IP_GUID type AC_GUID
    returning
      value(EP_RESULT) type ABAP_BOOL .
  methods CONFIRM_ALERT_GROUP_BY_GUID
    importing
      !IP_GUID type AC_GUID
    returning
      value(EP_RC) type SY-SUBRC .
  class-methods GET_ACTION
    importing
      !IP_PATH type STRING
    returning
      value(EP_RESULT) type CHAR64 .
  class-methods GET_ALERT_GUID
    importing
      !IP_PATH type STRING
    returning
      value(EP_RESULT) type CHAR32 .
ENDCLASS.



CLASS ZCL_ALERT_ACTIONS_HANDLER IMPLEMENTATION.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Private Method ZCL_ALERT_ACTIONS_HANDLER->CONFIRM_ALERT_GROUP_BY_GUID
* +-------------------------------------------------------------------------------------------------+
* | [--->] IP_GUID                        TYPE        AC_GUID
* | [<-()] EP_RC                          TYPE        SY-SUBRC
* +--------------------------------------------------------------------------------------</SIGNATURE>
  method confirm_alert_group_by_guid.

    data: lt_suppressed_alerts type e2ea_t_guid,
          ls_suppressed_alert  type ac_guid,
          lo_acc               type ref to cl_alert_consumer_connector,
          lv_confirmation_text type string,
          lv_count             type i,
          lr_db_exception      type ref to cx_alert_consm_database,
          lr_consm_exception   type ref to cx_alert_consm_connector,
          lv_is_alert_exist    type abap_bool.

    ep_rc = 1.


    lv_is_alert_exist = is_alert_exist( ip_guid ) .

    if lv_is_alert_exist eq abap_false.

      return.

    endif.

    lv_confirmation_text = zcl_servess_alerts_integration=>get_alrt_rest_int_param_value( 'ALERT_CONFIRM_TEXT' ).

    ls_suppressed_alert = ip_guid.

    append ls_suppressed_alert to lt_suppressed_alerts.



    try.

        create object lo_acc.

        lo_acc->if_alert_consumer_connector~confirm_alert_groups(
          exporting
            it_algroupid = lt_suppressed_alerts
            i_comments = lv_confirmation_text
            i_category_id = 'Z01'
            i_classification_id = 'Z01'
          importing
            e_incident_alert  = lv_count ).

      catch cx_alert_consm_database into lr_db_exception.
        ep_rc = 1.
      catch cx_alert_consm_connector into lr_consm_exception.
        ep_rc = 1.

    endtry.

    if sy-subrc eq 0.
      ep_rc = 0.
    endif.

  endmethod.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Static Private Method ZCL_ALERT_ACTIONS_HANDLER=>GET_ACTION
* +-------------------------------------------------------------------------------------------------+
* | [--->] IP_PATH                        TYPE        STRING
* | [<-()] EP_RESULT                      TYPE        CHAR64
* +--------------------------------------------------------------------------------------</SIGNATURE>
  method GET_ACTION.

    search ip_path for '&'.

    if sy-subrc eq 0.

      ep_result = substring_before( val = ip_path sub = '&' ).

    endif. "if sy-subrc eq 0

  endmethod.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Static Private Method ZCL_ALERT_ACTIONS_HANDLER=>GET_ALERT_GUID
* +-------------------------------------------------------------------------------------------------+
* | [--->] IP_PATH                        TYPE        STRING
* | [<-()] EP_RESULT                      TYPE        CHAR32
* +--------------------------------------------------------------------------------------</SIGNATURE>
  method GET_ALERT_GUID.


    search ip_path for 'alert_guid='.

    if sy-subrc eq 0.

      ep_result = substring_after( val = ip_path sub = 'alert_guid=' ).

      if strlen( ep_result ) ne 32.
        clear ep_result.
      endif.

    endif. "if sy-subrc eq 0


  endmethod.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Public Method ZCL_ALERT_ACTIONS_HANDLER->IF_HTTP_EXTENSION~HANDLE_REQUEST
* +-------------------------------------------------------------------------------------------------+
* | [--->] SERVER                         TYPE REF TO IF_HTTP_SERVER
* +--------------------------------------------------------------------------------------</SIGNATURE>
  method if_http_extension~handle_request.

    types: begin of ty_json_res,
             response type string,
           end of ty_json_res .


    data:lv_path            type string,
         lv_action          type char64,
         lv_parameter       type string,
         lv_alert_guid      type ac_guid,
         lv_rc              type sy-subrc,
         lv_http_code       type i,
         lv_reason          type string,
         ls_json_res_obj    type ty_json_res,
         lv_json_res_str    type string,
         lr_json_serializer type ref to zcl_json_serializer.


    " Default response

    lv_rc = 1.
    lv_http_code = 500.
    lv_reason = 'Internal Server Error'.
    ls_json_res_obj-response = 'Alert confirmation error'.

    " Getting path

    lv_path = server->request->get_header_field( name = '~query_string' ).

    lv_action = get_action( lv_path ).

    translate lv_action to upper case.

    case lv_action.

      when 'CONFIRM'.

        lv_alert_guid = get_alert_guid( lv_path ).

        if lv_alert_guid is not initial.

          lv_rc = confirm_alert_group_by_guid( lv_alert_guid ).

        endif. " if lv_alert_guid is not initial

    endcase.

    " Producing output

    if lv_rc eq 0.

      lv_http_code = 200.
      concatenate 'Alert' lv_alert_guid 'confirmed successfully' into ls_json_res_obj-response separated by space.
      lv_reason = 'OK'.

    endif.

    server->response->set_status( code = lv_http_code reason = lv_reason ).

    " Executing serialization

    create object lr_json_serializer
      exporting
        data = ls_json_res_obj.

    lr_json_serializer->serialize( ).
    lv_json_res_str = lr_json_serializer->get_data( ).

    server->response->set_cdata( data = lv_json_res_str ).



  endmethod.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Static Private Method ZCL_ALERT_ACTIONS_HANDLER=>IS_ALERT_EXIST
* +-------------------------------------------------------------------------------------------------+
* | [--->] IP_GUID                        TYPE        AC_GUID
* | [<-()] EP_RESULT                      TYPE        ABAP_BOOL
* +--------------------------------------------------------------------------------------</SIGNATURE>
  method is_alert_exist.

    ep_result = abap_false.

    select count( * ) up to 1 rows from e2ea_alertgroup
       where algroup_id = ip_guid.

    if sy-subrc eq 0.
      ep_result = abap_true.
    endif.

  endmethod.
ENDCLASS.