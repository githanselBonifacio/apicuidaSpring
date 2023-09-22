package co.com.sura.autoagendador;

import lombok.Getter;

@Getter
public enum IdRegional {
    BARRANQUILLA("427"),
    MEDELLIN("4292"),
    BOGOTA("586"),
    RIONEGRO("5608"),
    CALI("834");

    private final String codigo;

    IdRegional(String codigo) {
        this.codigo = codigo;
    }
    public static IdRegional getByIdCiudad(String idRegional) {
        for (IdRegional ciudad : IdRegional.values()) {
            if (ciudad.codigo.equals(idRegional)) {
                return ciudad;
            }
        }
        throw new IllegalArgumentException("Invalid idRegional: " + idRegional);
    }
}
