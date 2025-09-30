FUNCTION ZBTE_F110_00001830
  IMPORTING
    I_BUDAT LIKE F110C-BUDAT OPTIONAL
    I_NEDAT LIKE F110V-NEDAT OPTIONAL
    I_FDEBI LIKE F110V-FDEBI OPTIONAL
    I_TRACE LIKE TRCOPT OPTIONAL
  CHANGING
    C_REGUH TYPE REGUH
  TABLES
    T_REGUP LIKE REGUP.



  " mark all invoices that do not have the invoice category as matching the invoice category as excluded
  DATA: lt_bkpf  TYPE STANDARD TABLE OF bkpf,
        lt_rbkp  TYPE STANDARD TABLE OF rbkp,
        lt_regup TYPE STANDARD TABLE OF regup.

  DO.
    BREAK esuharto.
  ENDDO.

  " if the run code does not exist in the inv category codes - bypass the check -  E002
  SELECT invoice_category
    FROM zfi_inv_pymt_run
   WHERE payment_run_id = @c_reguh-laufi
    INTO TABLE @DATA(lt_inv_cat).
  
  IF sy-subrc = 0.
    APPEND LINES OF t_regup TO lt_regup.
    SORT lt_regup BY zlsch.
    READ TABLE lt_regup INDEX 1 ASSIGNING FIELD-SYMBOL(<fs_regup>).
    IF sy-subrc = 0.
      DATA(lv_payment_method) = <fs_regup>-zlsch.
      IF lv_payment_method = c_reguh-laufi+3(1).  " if payment method from line item match with payment method from run id
        " check in line item if any different payment method then raise an exception
        DATA(lv_consistent) = abap_true.
        LOOP AT lt_regup ASSIGNING <fs_regup>.
          IF lv_payment_method NE <fs_regup>-zlsch.
            lv_consistent = abap_false.
            EXIT.
          ENDIF.
        ENDLOOP.
        " Continue processing only if payment method is consistent
        IF lv_consistent = abap_false.
          " Payment method inconsistency - exit processing
        ENDIF.
      ELSE.
        " Payment method mismatch - exit processing
      ENDIF.
    ENDIF.
  ENDIF.
*-------------------------------------------
  SELECT bukrs, belnr, gjahr, awkey
    FROM bkpf
     FOR ALL ENTRIES IN @t_regup
   WHERE bukrs = @t_regup-bukrs
     AND belnr = @t_regup-belnr
     AND gjahr = @t_regup-gjahr
    INTO TABLE @lt_bkpf.
  
  IF sy-subrc = 0.
    SORT lt_bkpf BY bukrs belnr gjahr.
*--------------------------------------------
    SELECT belnr, gjahr, zz1_invoicecategory_mih
      FROM rbkp
      FOR ALL ENTRIES IN @lt_bkpf
      WHERE belnr   = @lt_bkpf-awkey+0(10)
        AND gjahr   = @lt_bkpf-gjahr
      ORDER BY PRIMARY KEY
      INTO TABLE @lt_rbkp.
    
    IF sy-subrc = 0.
      SORT lt_rbkp BY belnr gjahr.
    ENDIF.
  ENDIF.
*------------------------------------------------------
  " Filter invoice categories - build new list instead of deleting
  DATA: lt_filtered_rbkp TYPE STANDARD TABLE OF rbkp.
  
  LOOP AT lt_rbkp ASSIGNING FIELD-SYMBOL(<fs_rbkp>).
    READ TABLE lt_inv_cat WITH KEY invoice_category = <fs_rbkp>-zz1_invoicecategory_mih TRANSPORTING NO FIELDS.
    IF sy-subrc = 0.
      APPEND <fs_rbkp> TO lt_filtered_rbkp.
    ENDIF.
  ENDLOOP.
  
  " Replace original table with filtered results
  lt_rbkp = lt_filtered_rbkp.
*-------------------------------------------------------
  LOOP AT t_regup ASSIGNING FIELD-SYMBOL(<regup>).
    READ TABLE lt_bkpf ASSIGNING FIELD-SYMBOL(<bkpf>)
         WITH KEY bukrs = <regup>-bukrs
                  belnr = <regup>-belnr
                  gjahr = <regup>-gjahr
         BINARY SEARCH.
    READ TABLE lt_rbkp ASSIGNING <fs_rbkp>
         WITH KEY belnr = <bkpf>-awkey+0(10)
                  gjahr = <bkpf>-gjahr
         BINARY SEARCH.
    IF sy-subrc IS NOT INITIAL.  " if its not in the invoice category exclude it
      <regup>-xigno = 'X'.
    ENDIF.
  ENDLOOP.

  SORT lt_bkpf BY bukrs belnr gjahr.


  " check if line max from table and loop from 1 to max limit excluding the rest - and then reschedule a job to restart the process with the last run identifier + 1. F110S
  "--- Cap processing at max limit and exclude the rest, then reschedule next run (F110S)

  " Get max limit of the proposal from table ztb_appaylimit for each company.

  DATA lv_lines TYPE i.

  SELECT SINGLE invoice_limit
    FROM ztb_appaylimit
   WHERE bukrs = @c_reguh-zbukr
     AND zlsch = @lv_payment_method
    INTO @DATA(lv_max_limit).

  lv_lines = lines( lt_bkpf ).

  " Only apply limits if we exceed the maximum
  IF lv_lines > lv_max_limit.
*---------------------------------------------------------------------
    READ TABLE lt_bkpf ASSIGNING FIELD-SYMBOL(<bkpf_10k>) INDEX lv_max_limit.
    IF sy-subrc IS INITIAL.
      " Mark every item from lv_max_limit onwards as excluded in T_REGUP
      " Sort to allow binary search. If caller cares about order, keep a copy and restore if needed.
      SORT t_regup BY bukrs belnr gjahr.
      LOOP AT lt_bkpf ASSIGNING FIELD-SYMBOL(<bkpf_cap>) FROM lv_max_limit.
        READ TABLE t_regup ASSIGNING <regup> with key bukrs = <bkpf_cap>-bukrs belnr = <bkpf_cap>-belnr gjahr = <bkpf_cap>-gjahr BINARY SEARCH.
        IF sy-subrc = 0.
          <regup>-xigno = 'X'.
        ENDIF.
      ENDLOOP.
    ENDIF.
  ENDIF.
  ENDFUNCTION.