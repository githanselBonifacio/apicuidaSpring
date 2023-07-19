package co.com.sura.entity.agenda;

import co.com.sura.entity.remision.*;
import reactor.core.publisher.Flux;
import reactor.core.publisher.Mono;

import java.time.LocalDate;

public interface AgendaRepository {
    //profesionales
    Flux<Profesional> consultarProfesionales();
    Flux<Profesional> consultarProfesionalByTurnoCiudad(LocalDate fechaTurno, String idCiudad);

    Flux<Profesional> consultarProfesionalFromTurnoCiudad(
            LocalDate fechaTurno, String idCiudad, Integer idHorarioTurno);

    Mono<Void> asignarProfesionalTurno (LocalDate fechaTurno,Integer idHorarioTurno,String idProfesional);
    Mono<Void> desasignarProfesionalTurno (LocalDate fechaTurno,Integer idHorarioTurno,String idProfesional);

    Flux<Profesional> consultarProfesionalesByIdCiudad(String idCiudad);
    Flux<Actividad> consultarActividadesByProfesionalesCiudadHorarioTurno(
            LocalDate fechaTurno,
            Integer idHorarioTurno,
            String idCiudad
    );

    Mono<Profesional> crearProfesional(Profesional profesional);
    Mono<Profesional> actualizarProfesional(Profesional profesional);

    //citas
    Flux<Cita>        consultarCitasByTurnoCiudad(LocalDate fechaTurno, Integer idHorarioTurno, String idCiudad);
    Mono<Void>        agendarToProfesional(String idCita, String idProfesional);
    Mono<Void>        desagendarToProfesional(String idCita);

    //tratamientos
    Flux<Tratamiento> consultarTratamientoByCitas(String idCita);

    //procedimientos
    //curaciones
    Flux<Curacion> consultarCuracionesByCitas(String idCita);

    //Canalizaciones
    Flux<Canalizacion> consultarCanalizacionesByCitas(String idCita);

    //Fototerapias
    Flux<Fototerapia> consultarFototerapiasByCitas(String idCita);

    //Secreciones
    Flux<Secrecion> consultarSecrecionesByCitas(String idCita);

    //sondaje
    Flux<Sondaje> consultarSondajesByCitas(String idCita);

    //toma de muestras
    Flux<TomaMuestra> consultarTomaMuestrasByCitas(String idCita);

    //soporte nutricionales
    Flux<SoporteNutricional> consultarSoporteNutricionalesByCitas(String idCita);
}
