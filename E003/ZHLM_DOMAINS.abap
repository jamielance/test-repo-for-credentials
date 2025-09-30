*&---------------------------------------------------------------------*
*& Domain: ZHLM_DOM_WEX_UUID
*& Purpose: Domain for WEX UUID (32 character string)
*& Namespace: ZHLM
*& Package: ZHLM_AP_WEX
*&---------------------------------------------------------------------*

@EndUserText.label : 'WEX UUID Domain'
@AbapCatalog.enhancementCategory : #NOT_EXTENSIBLE
@AbapCatalog.tableCategory : #TRANSPARENT
@AbapCatalog.deliveryClass : #C
@AbapCatalog.dataMaintenance : #RESTRICTED
define domain zhlm_dom_wex_uuid {
  @EndUserText.label : 'WEX UUID'
  @AbapCatalog.enhancementCategory : #NOT_EXTENSIBLE
  char32;
}
