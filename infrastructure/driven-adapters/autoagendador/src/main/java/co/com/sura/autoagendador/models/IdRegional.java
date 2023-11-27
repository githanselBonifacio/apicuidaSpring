package co.com.sura.autoagendador.models;

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
    public static IdRegional getByIdRegional(String idRegional) {
        for (IdRegional regional : IdRegional.values()) {
            if (regional.codigo.equals(idRegional)) {
                return regional;
            }
        }
        throw new IllegalArgumentException("Invalid idRegional: " + idRegional);
    }
}
