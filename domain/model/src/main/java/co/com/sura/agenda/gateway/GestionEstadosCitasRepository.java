package co.com.sura.agenda.gateway;

import co.com.sura.agenda.entity.Cita;
import reactor.core.publisher.Mono;

import java.time.LocalDate;
import java.time.LocalDateTime;
import java.util.List;

public interface GestionEstadosCitasRepository {


    Mono<Boolean> agendarToProfesional(String idCita, String idProfesional, LocalDateTime fechaProgramada,
                                       Integer idHorarioTurno, String idRegional);

    Mono<Boolean> desagendarToProfesional(String idCita, String idProfesional, LocalDate fechaTurno,
                                          Integer idHorarioTurno, String idRegional);

    Mono<Boolean> confirmarCita(String idCita);

    Mono<Integer> confirmarTodasCitasTurno(List<Cita> citas);
    Mono<Boolean> iniciarAtencionCita(String idCita);
    Mono<Boolean> finalizarAtencionCita(String idCita);

    Mono<Boolean> cancelarCita(String idCita,Integer idMotivoCancelacion);

}
