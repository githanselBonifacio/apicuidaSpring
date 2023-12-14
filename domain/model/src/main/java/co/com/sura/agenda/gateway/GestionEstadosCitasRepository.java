package co.com.sura.agenda.gateway;

import reactor.core.publisher.Mono;

import java.time.LocalDate;
import java.time.LocalDateTime;

public interface GestionEstadosCitasRepository {


    Mono<Boolean> agendarToProfesional(String idCita, String idProfesional, LocalDateTime fechaProgramada,
                                       Integer idHorarioTurno, String idRegional);

    Mono<Boolean> desagendarToProfesional(String idCita, String idProfesional, LocalDate fechaTurno,
                                          Integer idHorarioTurno, String idRegional);

    Mono<Boolean> confirmarCita(String idCita);
    Mono<Boolean> iniciarAtencionCita(String idCita);
    Mono<Boolean> finalizarAtencionCita(String idCita);

}
