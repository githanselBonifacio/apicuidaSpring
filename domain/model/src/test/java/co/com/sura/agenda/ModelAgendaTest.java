package co.com.sura.agenda;

import co.com.sura.genericos.EstadosCita;
import org.junit.jupiter.api.Test;


import static org.junit.jupiter.api.Assertions.assertEquals;


class ModelAgendaTest {

    @Test
    void validarEstadoCita(){
        String resultadoSinAgendar= EstadosCita.getNombreEstado(EstadosCita.SIN_AGENDAR);
        assertEquals("Sin Agendar", resultadoSinAgendar);

        String resultadoAgendada = EstadosCita.getNombreEstado(EstadosCita.AGENDADA);
        assertEquals("Agendada", resultadoAgendada);

        String resultadoConfirmada = EstadosCita.getNombreEstado(EstadosCita.CONFIRMADA);
        assertEquals("Confirmada", resultadoConfirmada);

        String resultadoProgreso= EstadosCita.getNombreEstado(EstadosCita.EN_PROGRESO);
        assertEquals("En Progreso", resultadoProgreso);

        String resultadoCancelada = EstadosCita.getNombreEstado(EstadosCita.CANCELADA);
        assertEquals("Cancelada", resultadoCancelada);

        String resultadoFinalizada = EstadosCita.getNombreEstado(EstadosCita.FINALIZADA);
        assertEquals("Finalizada", resultadoFinalizada);
    }
}
