  METHOD main.
    DATA(lo_dog_container) = NEW zcl_animal_container( iv_concrete_type = 'ZCL_DOG' ).
* concrete type must be ZCL_DOG or its subclass!
    DATA(lo_cat_container) = NEW zcl_animal_container( iv_concrete_type = 'ZCL_CAT' ).

    DATA(lo_cat) = NEW zcl_cat( ).
    DATA(lo_dog) = NEW zcl_dog( ).
    DATA(lo_toydog) = NEW zcl_toydog( ).

* only dog or dog subclass instance is allowed for insertion
    lo_dog_container->add( lo_cat ).
    lo_dog_container->add( lo_dog ).
    lo_dog_container->add( lo_toydog ).

  ENDMETHOD.