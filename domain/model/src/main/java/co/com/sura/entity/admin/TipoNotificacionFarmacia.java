package co.com.sura.entity.admin;

public enum TipoNotificacionFarmacia {
    APLICACION_MEDICAMENTO("Aplicacion de medicamento"),
    SOPORTE_NUTRICIONAL("Soporte nutricional");

    private final String tipo;

    TipoNotificacionFarmacia(String tipo) {
        this.tipo = tipo;
    }
    public String getTipo() {
        return tipo;
    }


}
