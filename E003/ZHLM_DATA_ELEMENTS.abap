*&---------------------------------------------------------------------*
*& Data Element: ZHLM_DE_WEX_UUID
*& Purpose: Data element for WEX UUID based on domain
*& Namespace: ZHLM
*& Package: ZHLM_AP_WEX
*&---------------------------------------------------------------------*

@EndUserText.label : 'WEX UUID'
@AbapCatalog.enhancementCategory : #NOT_EXTENSIBLE
@AbapCatalog.tableCategory : #TRANSPARENT
@AbapCatalog.deliveryClass : #C
@AbapCatalog.dataMaintenance : #RESTRICTED
define data element zhlm_de_wex_uuid {
  @EndUserText.label : 'WEX UUID'
  @AbapCatalog.enhancementCategory : #NOT_EXTENSIBLE
  zhlm_dom_wex_uuid;
}
