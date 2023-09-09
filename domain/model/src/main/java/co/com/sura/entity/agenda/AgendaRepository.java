package co.com.sura.entity.agenda;

import co.com.sura.entity.moviles.Desplazamiento;
import co.com.sura.entity.remision.*;
import reactor.core.publisher.Flux;
import reactor.core.publisher.Mono;

import java.time.LocalDate;
import java.time.LocalDateTime;

public interface AgendaRepository {
    //profesionales
    Flux<Profesional>   consultarProfesionales();
    Flux<Profesional>   consultarProfesionalByTurnoCiudad(LocalDate fechaTurno, String idCiudad);

    Flux<Profesional>   consultarProfesionalFromTurnoCiudad(
                            LocalDate fechaTurno, String idCiudad, Integer idHorarioTurno);

    Mono<Boolean>           asignarProfesionalTurno (LocalDate fechaTurno,Integer idHorarioTurno,String idProfesional);
    Mono<Boolean>           desasignarProfesionalTurno (LocalDate fechaTurno,Integer idHorarioTurno,String idProfesional);

    Flux<Profesional>    consultarProfesionalesByIdCiudad(String idCiudad);
    Flux<Actividad>      consultarActividadesByProfesionalesCiudadHorarioTurno(
                            LocalDate fechaTurno,
                            Integer idHorarioTurno,
                            String idCiudad
                         );

    Mono<Profesional>     crearProfesional(Profesional profesional);
    Mono<Profesional>     actualizarProfesional(Profesional profesional);

    //citas
    Flux<Cita>            consultarCitasByTurnoCiudad(LocalDate fechaTurno, Integer idHorarioTurno, String idCiudad);
    Mono<Boolean>            reprogramarCita(LocalDateTime fechaProgramada, String idCita);
    Mono<Boolean>            agendarToProfesional(String idCita, String idProfesional);
    Mono<Boolean>            desagendarToProfesional(String idCita);
    Mono<Boolean>         desagendarTurnocompleto(LocalDate fechaTurno, Integer idHorarioTurno,String idCiudad);
    Mono<Boolean>            autoagendarTurnoCompleto(LocalDate fechaTurno, Integer idHorarioTurno, String idCiudad);

    //desplazamientos
    Flux<Desplazamiento>   consultarDesplazamientoByCitaPartida(
                                        LocalDate fechaProgramada,
                                        Integer idHorarioTurno,
                                        String idCiudad);

    Mono<Boolean>              calcularDesplazamientoCitaByProfesional(
                                        LocalDate fechaProgramada,
                                        Integer idHorarioTurno,
                                        String idCiudad,
                                        String idProfesional);
    //tratamientos
    Flux<Tratamiento>       consultarTratamientoByCitas(String idCita);

    //procedimientos
    Mono<Procedimientos>    consultarProcedimientosByIdCita(String idCita);
    //curaciones
    Flux<Curacion>          consultarCuracionesByCitas(String idCita);

    //Canalizaciones
    Flux<Canalizacion>      consultarCanalizacionesByCitas(String idCita);

    //Fototerapias
    Flux<Fototerapia>       consultarFototerapiasByCitas(String idCita);

    //Secreciones
    Flux<Secrecion>          consultarSecrecionesByCitas(String idCita);

    //sondaje
    Flux<Sondaje>            consultarSondajesByCitas(String idCita);

    //toma de muestras
    Flux<TomaMuestra>        consultarTomaMuestrasByCitas(String idCita);

    //soporte nutricionales
    Flux<SoporteNutricional> consultarSoporteNutricionalesByCitas(String idCita);
}
