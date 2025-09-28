*&---------------------------------------------------------------------*
*& Function Module: ZCOS_SETUP_FEATURE_TOGGLE
*& Description: Setup feature toggle for E003 Cost of Sales Auto Posting
*&---------------------------------------------------------------------*
FUNCTION zcos_setup_feature_toggle.
*"----------------------------------------------------------------------
*"*"Local Interface:
*"----------------------------------------------------------------------

  DATA: ls_tvarvc TYPE tvarvc.

  " Set up feature toggle parameter
  ls_tvarvc-name = 'ZCOS_E003_ACTIVE'.
  ls_tvarvc-type = 'P'.
  ls_tvarvc-numb = '000000'.
  ls_tvarvc-low = 'X'. " Active by default
  ls_tvarvc-text = 'E003 Cost of Sales Auto Posting - Active Flag'.

  " Insert or update TVARVC entry
  MODIFY tvarvc FROM ls_tvarvc.

  COMMIT WORK.

  MESSAGE s001(zcos) WITH 'Feature toggle setup completed'.

ENDFUNCTION.

*&---------------------------------------------------------------------*
*& Function Module: ZCOS_DEACTIVATE_FEATURE
*& Description: Deactivate E003 Cost of Sales Auto Posting
*&---------------------------------------------------------------------*
FUNCTION zcos_deactivate_feature.
*"----------------------------------------------------------------------
*"*"Local Interface:
*"----------------------------------------------------------------------

  " Deactivate feature toggle
  UPDATE tvarvc SET low = ''
    WHERE name = 'ZCOS_E003_ACTIVE'.

  COMMIT WORK.

  MESSAGE s002(zcos) WITH 'Feature deactivated'.

ENDFUNCTION.
