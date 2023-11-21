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
}
