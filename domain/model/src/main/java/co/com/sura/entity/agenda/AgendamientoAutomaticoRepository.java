package co.com.sura.entity.agenda;

import co.com.sura.entity.moviles.Desplazamiento;
import reactor.core.publisher.Flux;
import reactor.core.publisher.Mono;

import java.time.LocalDate;

public interface AgendamientoAutomaticoRepository {
    Mono<Boolean>         desagendarTurnoCompleto(LocalDate fechaTurno, Integer idHorarioTurno, String idCiudad);
    Mono<Boolean>         autoagendarTurnoCompleto(LocalDate fechaTurno, Integer idHorarioTurno, String idCiudad);
    Flux<Desplazamiento>  consultarDesplazamientoByCitaPartida(
                                                                LocalDate fechaProgramada,
                                                                Integer idHorarioTurno,
                                                                String idCiudad);

    Mono<Boolean>         calcularDesplazamientoCitaByProfesional(
                                                                    LocalDate fechaProgramada,
                                                                    Integer idHorarioTurno,
                                                                    String idCiudad,
                                                                    String idProfesional);
}
