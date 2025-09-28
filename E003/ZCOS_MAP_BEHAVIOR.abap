*&---------------------------------------------------------------------*
*& Behavior Definition: ZCOS_MAP_BDEF
*& Description: Behavior Definition for COS Mapping
*&---------------------------------------------------------------------*
managed implementation in class zbp_zcos_map_cds unique;
strict ( 2 );

define behavior for ZCOS_MAP_CDS alias COSMapping
persistent table zcos_map
lock master
authorization master ( instance )
etag master created_at
{
  // Administrative fields (read-only)
  field ( readonly ) created_by, created_at, changed_by, changed_at;

  // Standard operations
  create;
  update;
  delete;

  // Validation
  validation validateData on save { field bukrs, trigger_gl, product_code, valid_from, valid_to, sales_gl, cos_gl; }
  validation validateValidityPeriod on save { field valid_from, valid_to; }
  validation validateGLAccounts on save { field trigger_gl, sales_gl, cos_gl; }

  // Actions
  action ( features : instance ) activate;
  action ( features : instance ) deactivate;
  action ( features : instance ) copy;

  // Internal actions
  internal action prepare;
  internal action cleanup;
}

*&---------------------------------------------------------------------*
*& Behavior Implementation: ZBP_ZCOS_MAP_CDS
*& Description: Behavior Implementation for COS Mapping
*&---------------------------------------------------------------------*
CLASS zbp_zcos_map_cds DEFINITION
  PUBLIC
  ABSTRACT
  FINAL
  FOR BEHAVIOR OF ZCOS_MAP_CDS.

  PUBLIC SECTION.
    INTERFACES if_oo_adt_classrun.
  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.

CLASS zbp_zcos_map_cds IMPLEMENTATION.

  METHOD validateData.
    " Validate required fields
    LOOP AT keys INTO DATA(key).
      READ ENTITIES OF ZCOS_MAP_CDS IN LOCAL MODE
        ENTITY COSMapping
        FIELDS ( bukrs trigger_gl product_code valid_from valid_to sales_gl cos_gl )
        WITH VALUE #( ( %key = key ) )
        RESULT DATA(lt_cos_mapping).

      LOOP AT lt_cos_mapping INTO DATA(ls_cos_mapping).
        " Check required fields
        IF ls_cos_mapping-bukrs IS INITIAL.
          APPEND VALUE #( %key = key %msg = new_message( id = 'ZCOS' number = '001' severity = if_abap_behv_message=>severity-error ) ) TO reported-cosmapping.
        ENDIF.

        IF ls_cos_mapping-trigger_gl IS INITIAL.
          APPEND VALUE #( %key = key %msg = new_message( id = 'ZCOS' number = '002' severity = if_abap_behv_message=>severity-error ) ) TO reported-cosmapping.
        ENDIF.

        IF ls_cos_mapping-product_code IS INITIAL.
          APPEND VALUE #( %key = key %msg = new_message( id = 'ZCOS' number = '003' severity = if_abap_behv_message=>severity-error ) ) TO reported-cosmapping.
        ENDIF.

        IF ls_cos_mapping-valid_from IS INITIAL.
          APPEND VALUE #( %key = key %msg = new_message( id = 'ZCOS' number = '004' severity = if_abap_behv_message=>severity-error ) ) TO reported-cosmapping.
        ENDIF.

        IF ls_cos_mapping-valid_to IS INITIAL.
          APPEND VALUE #( %key = key %msg = new_message( id = 'ZCOS' number = '005' severity = if_abap_behv_message=>severity-error ) ) TO reported-cosmapping.
        ENDIF.

        IF ls_cos_mapping-sales_gl IS INITIAL.
          APPEND VALUE #( %key = key %msg = new_message( id = 'ZCOS' number = '006' severity = if_abap_behv_message=>severity-error ) ) TO reported-cosmapping.
        ENDIF.

        IF ls_cos_mapping-cos_gl IS INITIAL.
          APPEND VALUE #( %key = key %msg = new_message( id = 'ZCOS' number = '007' severity = if_abap_behv_message=>severity-error ) ) TO reported-cosmapping.
        ENDIF.
      ENDLOOP.
    ENDLOOP.
  ENDMETHOD.

  METHOD validateValidityPeriod.
    " Validate validity period
    LOOP AT keys INTO DATA(key).
      READ ENTITIES OF ZCOS_MAP_CDS IN LOCAL MODE
        ENTITY COSMapping
        FIELDS ( valid_from valid_to )
        WITH VALUE #( ( %key = key ) )
        RESULT DATA(lt_cos_mapping).

      LOOP AT lt_cos_mapping INTO DATA(ls_cos_mapping).
        IF ls_cos_mapping-valid_from > ls_cos_mapping-valid_to.
          APPEND VALUE #( %key = key %msg = new_message( id = 'ZCOS' number = '008' severity = if_abap_behv_message=>severity-error ) ) TO reported-cosmapping.
        ENDIF.
      ENDLOOP.
    ENDLOOP.
  ENDMETHOD.

  METHOD validateGLAccounts.
    " Validate G/L accounts exist
    LOOP AT keys INTO DATA(key).
      READ ENTITIES OF ZCOS_MAP_CDS IN LOCAL MODE
        ENTITY COSMapping
        FIELDS ( trigger_gl sales_gl cos_gl )
        WITH VALUE #( ( %key = key ) )
        RESULT DATA(lt_cos_mapping).

      LOOP AT lt_cos_mapping INTO DATA(ls_cos_mapping).
        " Check trigger G/L exists
        SELECT SINGLE saknr FROM ska1 INTO @DATA(lv_saknr)
          WHERE saknr = @ls_cos_mapping-trigger_gl.
        IF sy-subrc <> 0.
          APPEND VALUE #( %key = key %msg = new_message( id = 'ZCOS' number = '009' severity = if_abap_behv_message=>severity-error ) ) TO reported-cosmapping.
        ENDIF.

        " Check sales G/L exists
        SELECT SINGLE saknr FROM ska1 INTO lv_saknr
          WHERE saknr = @ls_cos_mapping-sales_gl.
        IF sy-subrc <> 0.
          APPEND VALUE #( %key = key %msg = new_message( id = 'ZCOS' number = '010' severity = if_abap_behv_message=>severity-error ) ) TO reported-cosmapping.
        ENDIF.

        " Check COS G/L exists
        SELECT SINGLE saknr FROM ska1 INTO lv_saknr
          WHERE saknr = @ls_cos_mapping-cos_gl.
        IF sy-subrc <> 0.
          APPEND VALUE #( %key = key %msg = new_message( id = 'ZCOS' number = '011' severity = if_abap_behv_message=>severity-error ) ) TO reported-cosmapping.
        ENDIF.
      ENDLOOP.
    ENDLOOP.
  ENDMETHOD.

  METHOD activate.
    " Activate mapping entry
    LOOP AT keys INTO DATA(key).
      READ ENTITIES OF ZCOS_MAP_CDS IN LOCAL MODE
        ENTITY COSMapping
        FIELDS ( deleted )
        WITH VALUE #( ( %key = key ) )
        RESULT DATA(lt_cos_mapping).

      LOOP AT lt_cos_mapping INTO DATA(ls_cos_mapping).
        MODIFY ENTITIES OF ZCOS_MAP_CDS IN LOCAL MODE
          ENTITY COSMapping
          UPDATE FIELDS ( deleted changed_by changed_at )
          WITH VALUE #( ( %key = key deleted = abap_false changed_by = sy-uname changed_at = cl_abap_tstmp=>utc2tstmp( cl_abap_tstmp=>get_utc( ) ) ) ).
      ENDLOOP.
    ENDLOOP.
  ENDMETHOD.

  METHOD deactivate.
    " Deactivate mapping entry
    LOOP AT keys INTO DATA(key).
      READ ENTITIES OF ZCOS_MAP_CDS IN LOCAL MODE
        ENTITY COSMapping
        FIELDS ( deleted )
        WITH VALUE #( ( %key = key ) )
        RESULT DATA(lt_cos_mapping).

      LOOP AT lt_cos_mapping INTO DATA(ls_cos_mapping).
        MODIFY ENTITIES OF ZCOS_MAP_CDS IN LOCAL MODE
          ENTITY COSMapping
          UPDATE FIELDS ( deleted changed_by changed_at )
          WITH VALUE #( ( %key = key deleted = abap_true changed_by = sy-uname changed_at = cl_abap_tstmp=>utc2tstmp( cl_abap_tstmp=>get_utc( ) ) ) ).
      ENDLOOP.
    ENDLOOP.
  ENDMETHOD.

  METHOD copy.
    " Copy mapping entry
    LOOP AT keys INTO DATA(key).
      READ ENTITIES OF ZCOS_MAP_CDS IN LOCAL MODE
        ENTITY COSMapping
        ALL FIELDS
        WITH VALUE #( ( %key = key ) )
        RESULT DATA(lt_cos_mapping).

      LOOP AT lt_cos_mapping INTO DATA(ls_cos_mapping).
        " Create new entry with modified key
        MODIFY ENTITIES OF ZCOS_MAP_CDS IN LOCAL MODE
          ENTITY COSMapping
          CREATE FIELDS ( bukrs trigger_gl product_code valid_from valid_to sales_gl cos_gl margin_pct created_by created_at )
          WITH VALUE #( ( bukrs = ls_cos_mapping-bukrs
                          trigger_gl = ls_cos_mapping-trigger_gl
                          product_code = |{ ls_cos_mapping-product_code }_COPY|
                          valid_from = sy-datum
                          valid_to = '99991231'
                          sales_gl = ls_cos_mapping-sales_gl
                          cos_gl = ls_cos_mapping-cos_gl
                          margin_pct = ls_cos_mapping-margin_pct
                          created_by = sy-uname
                          created_at = cl_abap_tstmp=>utc2tstmp( cl_abap_tstmp=>get_utc( ) ) ) ).
      ENDLOOP.
    ENDLOOP.
  ENDMETHOD.

ENDCLASS.
