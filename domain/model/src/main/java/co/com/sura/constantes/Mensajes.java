package co.com.sura.constantes;


import lombok.Getter;

@Getter
public enum Mensajes {
    PETICION_EXITOSA("Solicitud exitosa"),
    PETICION_FALLIDA("Error en petición"),

    REMISION_CREADA("Se ha creado la remisión exitosamente"),
    REMISION_EXISTENTE("Ya existe una remisión con el id ?"),
    REMISION_NO_EXISTENTE("No existe o ya fue egresada la remisión con el id ?"),
    REMISION_CITAS_PROGRESO("la remisión con el id ? tiene citas confirmadas o en progreso"),
    ERROR_CREAR_REMISION("error al crear remisión"),

    REMISION_EGRESADA("Se ha egresado la remisión"),
    ERROR_EGRESAR_REMISION("error a egresar remisión");



    private final String value;

    Mensajes(String value) {
        this.value = value;
    }
}

