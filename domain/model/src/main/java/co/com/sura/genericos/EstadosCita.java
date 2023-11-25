package co.com.sura.genericos;

import lombok.AllArgsConstructor;
import lombok.Getter;

@Getter
@AllArgsConstructor
public enum EstadosCita {
    SIN_AGENDAR(1),
    AGENDADA(2),
    CONFIRMADA(3),
    EN_PROGRESO(4),
    CANCELADA(5),
    FINALIZADA(6);
    private final int estado;

    public static  String getNombreEstado(EstadosCita estado) {
        var nombre = "";
        switch (estado) {
            case SIN_AGENDAR:
                nombre = "Sin Agendar";
                break;
            case AGENDADA:
                nombre = "Agendada";
                break;
            case CONFIRMADA:
                nombre = "Confirmada";
                break;
            case EN_PROGRESO:
                nombre = "En Progreso";
                break;
            case CANCELADA:
                nombre = "Cancelada";
                break;
            case FINALIZADA:
                nombre = "Finalizada";
                break;
            default:
                nombre = "Estado Inválido";
                break;
        }
        return nombre;
    }
}
