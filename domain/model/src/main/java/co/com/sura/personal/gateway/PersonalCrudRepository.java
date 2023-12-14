package co.com.sura.personal.gateway;


import co.com.sura.moviles.entity.Movil;
import co.com.sura.personal.entity.Conductor;
import co.com.sura.personal.entity.Profesional;
import reactor.core.publisher.Flux;
import reactor.core.publisher.Mono;

import java.time.LocalDate;


public interface PersonalCrudRepository {
    //profesionales
    Flux<Profesional> consultarProfesionales();
    Flux<Profesional>                consultarProfesionalesByRegional(String idRegional);
    Mono<Profesional> crearProfesional(Profesional profesional);
    Mono<Profesional>                actualizarProfesional(Profesional profesional);

    Flux<Profesional> consultarProfesionalByTurnoRegional(LocalDate fechaTurno, String idRegional);

    Flux<Profesional> consultarProfesionalFromTurnoRegional(
            LocalDate fechaTurno, String idCiudad, Integer idHorarioTurno);

    Flux<Profesional>   consultarProfesionalesByIdRegional(String idRegional);

    //moviles
    Mono<Movil>                      crearMovil(Movil movil);
    Mono<Movil>                      actualizarMovil(Movil movil);
    Flux<Movil>                      consultarMoviles();
    Flux<Movil>                      consultarMovilesSinConductor();
    Flux<Movil>                      consultarMovilesByIdRegional(String idRegional);

    //conductores
    Mono<Conductor>                  crearConductor(Conductor profesional);
    Mono<Conductor>                  actualizarConductor(Conductor profesional);
    Flux<Conductor>                  consultarConductores();

}
