package co.com.sura.mapbox.repository.map.config;

public final class ConfigMapBox {
    public static final Integer MAXSIZE                       = 2;
    public static final Integer NUMERO_GENERACIONES           = 1000;
    public static final Integer SIZE_POBLACION_INICIAL        = 10;
    public static final Integer NUMERO_PADRES_EMPAREJADOS     = 5;
    public static final Integer HOLGURA_DEFECTO               = 1200;
    public static final double  PENALIZACION_HOLGURA_NEGATIVA = 1e6;

    private ConfigMapBox() {
    }
}
