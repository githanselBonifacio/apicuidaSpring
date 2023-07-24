package co.com.sura.autoagendador;

import lombok.Getter;

@Getter
public enum IdCiudad {
    BARRANQUILLA("427"),
    MEDELLIN("4292"),
    BOGOTA("586"),
    RIONEGRO("5608"),
    CALI("834");

    private final String codigo;

    IdCiudad(String codigo) {
        this.codigo = codigo;
    }
    public static IdCiudad getByIdCiudad(String idCiudad) {
        for (IdCiudad ciudad : IdCiudad.values()) {
            if (ciudad.codigo.equals(idCiudad)) {
                return ciudad;
            }
        }
        throw new IllegalArgumentException("Invalid idCiudad: " + idCiudad);
    }
}
