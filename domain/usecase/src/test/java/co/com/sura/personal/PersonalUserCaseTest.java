package co.com.sura.personal;


import co.com.sura.genericos.ResultadoActualizacionTurno;
import co.com.sura.moviles.entity.Movil;
import co.com.sura.personal.entity.*;
import co.com.sura.personal.gateway.PersonalCrudRepository;
import co.com.sura.personal.gateway.SecuenciasHorarioRepository;
import co.com.sura.remision.dto.EliminarTurnoProfesionalRequest;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.Mockito;
import org.mockito.junit.jupiter.MockitoExtension;
import reactor.core.publisher.Flux;
import reactor.core.publisher.Mono;
import reactor.test.StepVerifier;

import java.time.LocalDate;
import java.util.ArrayList;
import java.util.List;

@ExtendWith(MockitoExtension.class)
class PersonalUserCaseTest {
    private final LocalDate fechaTurno = LocalDate.of(2023,7,7);
    private final String idRegional = "427";
    @Mock
    private PersonalCrudRepository personalCrudRepositoryMock;

    @Mock
    private SecuenciasHorarioRepository secuenciasHorarioRepositoryMock;
    @InjectMocks
    private PersonalUseCase personalUseCaseMock;

    @Test
    void consultarProfesional(){
        List<Profesional> profesionales = new ArrayList<>();
        profesionales.add(Profesional.builder().build());
        profesionales.add(Profesional.builder().build());

        Mockito.when(personalCrudRepositoryMock.consultarProfesionales())
                .thenReturn(Flux.fromIterable(profesionales));

        Flux<Profesional> consultaProfesional = personalUseCaseMock
                .consultarProfesional();

        StepVerifier.create(consultaProfesional)
                .expectNextCount(2)
                .expectComplete()
                .verify();
    }

    @Test
    void consultarProfesionalByRegional(){
        List<Profesional> profesionales = new ArrayList<>();
        profesionales.add(Profesional.builder().build());
        profesionales.add(Profesional.builder().build());

        Mockito.when(personalCrudRepositoryMock.consultarProfesionalesByIdRegional(idRegional))
                .thenReturn(Flux.fromIterable(profesionales));

        Flux<Profesional> consultaProfesional = personalUseCaseMock
                .consultarProfesionalByRegional(idRegional);

        StepVerifier.create(consultaProfesional)
                .expectNextCount(2)
                .expectComplete()
                .verify();
    }

    @Test
    void crearProfesional(){
        Profesional profesional = Profesional.builder().build();


        Mockito.when(personalCrudRepositoryMock.crearProfesional(profesional))
                .thenReturn(Mono.just(profesional));

        Mono<Profesional> seCreoProfesional = personalUseCaseMock
                .crearProfesional(profesional);

        StepVerifier.create(seCreoProfesional)
                .expectNext(profesional)
                .expectComplete()
                .verify();
    }
    @Test
    void actualizarProfesional(){
        Profesional profesional = Profesional.builder().build();


        Mockito.when(personalCrudRepositoryMock.actualizarProfesional(profesional))
                .thenReturn(Mono.just(profesional));

        Mono<Profesional> seActualizoProfesional = personalUseCaseMock
                .actualizarProfesional(profesional);

        StepVerifier.create(seActualizoProfesional)
                .expectNext(profesional)
                .expectComplete()
                .verify();
    }

    @Test
    void consultarConductores(){
        List<Conductor> conductores = new ArrayList<>();
        conductores.add(Conductor.builder().build());
        conductores.add(Conductor.builder().build());

        Mockito.when(personalCrudRepositoryMock.consultarConductores())
                .thenReturn(Flux.fromIterable(conductores));

        Flux<Conductor> consultaConductores = personalUseCaseMock
                .consultarConductores();

        StepVerifier.create(consultaConductores)
                .expectNextCount(2)
                .expectComplete()
                .verify();
    }



    @Test
    void crearConductor(){
        Conductor conductor = Conductor.builder().build();


        Mockito.when(personalCrudRepositoryMock.crearConductor(conductor))
                .thenReturn(Mono.just(conductor));

        Mono<Conductor> seCreoConductor = personalUseCaseMock
                .crearConductor(conductor);

        StepVerifier.create(seCreoConductor)
                .expectNext(conductor)
                .expectComplete()
                .verify();
    }
    @Test
    void actualizarConductor(){
        Conductor conductor = Conductor.builder().build();


        Mockito.when(personalCrudRepositoryMock.actualizarConductor(conductor))
                .thenReturn(Mono.just(conductor));

        Mono<Conductor> seActualizoConductor = personalUseCaseMock
                .actualizarConductor(conductor);

        StepVerifier.create(seActualizoConductor)
                .expectNext(conductor)
                .expectComplete()
                .verify();
    }

    @Test
    void consultarMoviles(){
        List<Movil> moviles = new ArrayList<>();
        moviles.add(Movil.builder().build());
        moviles.add(Movil.builder().build());

        Mockito.when(personalCrudRepositoryMock.consultarMoviles())
                .thenReturn(Flux.fromIterable(moviles));

        Flux<Movil> consultaMoviles = personalUseCaseMock
                .consultarMoviles();

        StepVerifier.create(consultaMoviles)
                .expectNextCount(2)
                .expectComplete()
                .verify();
    }

    @Test
    void consultarMovilesByIdRegional(){
        List<Movil> moviles = new ArrayList<>();
        moviles.add(Movil.builder().build());
        moviles.add(Movil.builder().build());

        Mockito.when(personalCrudRepositoryMock.consultarMovilesByIdRegional(idRegional))
                .thenReturn(Flux.fromIterable(moviles));

        Flux<Movil> consultaMoviles = personalUseCaseMock
                .consultarMovilesByIdRegional(idRegional);

        StepVerifier.create(consultaMoviles)
                .expectNextCount(2)
                .expectComplete()
                .verify();
    }


    @Test
    void crearMovil(){
        Movil movil = Movil.builder().build();


        Mockito.when(personalCrudRepositoryMock.crearMovil(movil))
                .thenReturn(Mono.just(movil));

        Mono<Movil> crearMovil = personalUseCaseMock
                .crearMovil(movil);

        StepVerifier.create(crearMovil)
                .expectNext(movil)
                .expectComplete()
                .verify();
    }

    @Test
    void actualizarMovil(){
        Movil movil = Movil.builder().build();


        Mockito.when(personalCrudRepositoryMock.actualizarMovil(movil))
                .thenReturn(Mono.just(movil));

        Mono<Movil> seActualizaMovil = personalUseCaseMock
                .actualizarMovil(movil);

        StepVerifier.create(seActualizaMovil)
                .expectNext(movil)
                .expectComplete()
                .verify();
    }

    @Test
    void consultarProfesionalesTurnoByFechaTurnoIdRegional(){
        List<ProfesionalWithTurno> profesionales = new ArrayList<>();
        profesionales.add(ProfesionalWithTurno.builder().build());
        profesionales.add(ProfesionalWithTurno.builder().build());

        Mockito.when(secuenciasHorarioRepositoryMock.consultarHorariosProfesionales(fechaTurno.toString(),idRegional))
                .thenReturn(Flux.fromIterable(profesionales));

        Flux<ProfesionalWithTurno> profesionalWithTurno = personalUseCaseMock
                .consultarProfesionalesTurnoByFechaTurnoIdRegional(fechaTurno.toString(),idRegional);

        StepVerifier.create(profesionalWithTurno)
                .expectNextCount(2)
                .expectComplete()
                .verify();
    }


    @Test
    void eliminarTurnosProfesionalAccionMasiva(){

        List<ResultadoActualizacionTurno> resultadoEliminacion = new ArrayList<>();
        resultadoEliminacion.add(ResultadoActualizacionTurno.builder().build());
        resultadoEliminacion.add(ResultadoActualizacionTurno.builder().build());

        List<EliminarTurnoProfesionalRequest> requestEliminarTurnos = new ArrayList<>();
        requestEliminarTurnos.add(EliminarTurnoProfesionalRequest.builder().build());
        requestEliminarTurnos.add(EliminarTurnoProfesionalRequest.builder().build());

        Mockito.when(secuenciasHorarioRepositoryMock.eliminarTurnosProfesionalesAccionMasiva(requestEliminarTurnos))
                .thenReturn(Flux.fromIterable(resultadoEliminacion));

        Flux<ResultadoActualizacionTurno> respuestaResultadoEliminacion = personalUseCaseMock
                .eliminarTurnosProfesionalAccionMasiva(requestEliminarTurnos);

        StepVerifier.create(respuestaResultadoEliminacion)
                .expectNextCount(2)
                .expectComplete()
                .verify();
    }


    @Test
    void asignarTurnosProfesionalAccionMasiva(){

        List<TurnoProfesional> turnoProfesionales = new ArrayList<>();
        turnoProfesionales.add(TurnoProfesional.builder().build());
        turnoProfesionales.add(TurnoProfesional.builder().build());

        List<ResultadoActualizacionTurno> resultadoActualizacionTurnos = new ArrayList<>();
        resultadoActualizacionTurnos.add(ResultadoActualizacionTurno.builder().build());
        resultadoActualizacionTurnos.add(ResultadoActualizacionTurno.builder().build());

        Mockito.when(secuenciasHorarioRepositoryMock.asignarTurnosProfesionalesAccionMasiva(turnoProfesionales))
                .thenReturn(Flux.fromIterable(resultadoActualizacionTurnos));

        Flux<ResultadoActualizacionTurno> respuestaResultadoAsignacion = personalUseCaseMock
                .asignarTurnosProfesionalAccionMasiva(turnoProfesionales);

        StepVerifier.create(respuestaResultadoAsignacion)
                .expectNextCount(2)
                .expectComplete()
                .verify();
    }

    @Test
    void actualizarTurnosByProfesional(){

        List<TurnoProfesional> turnoProfesionales = new ArrayList<>();
        turnoProfesionales.add(TurnoProfesional.builder().build());
        turnoProfesionales.add(TurnoProfesional.builder().build());

        Mockito.when(secuenciasHorarioRepositoryMock.actualizarHorarioTurnoProfesionales(turnoProfesionales))
                .thenReturn(Mono.just(Boolean.TRUE));

        Mono<Boolean> respuestaActualizacionTurno= personalUseCaseMock
                .actualizarTurnosByProfesional(turnoProfesionales);

        StepVerifier.create(respuestaActualizacionTurno)
                .expectNext(Boolean.TRUE)
                .expectComplete()
                .verify();
    }

    @Test
    void consultarSecuenciasTurno(){
        List<SecuenciaTurno> secuenciaTurnos = new ArrayList<>();
        secuenciaTurnos.add(SecuenciaTurno.builder().build());
        secuenciaTurnos.add(SecuenciaTurno.builder().build());

        Mockito.when(secuenciasHorarioRepositoryMock.consultarSecuencias())
                .thenReturn(Flux.fromIterable(secuenciaTurnos));

        Flux<SecuenciaTurno> consultaSecuenciaTurnos = personalUseCaseMock
                .consultarSecuenciasTurno();

        StepVerifier.create(consultaSecuenciaTurnos)
                .expectNextCount(2)
                .expectComplete()
                .verify();
    }

    @Test
    void configurarSecuenciaTurno(){
        SecuenciaTurno secuenciaTurno = SecuenciaTurno.builder().build();


        Mockito.when(secuenciasHorarioRepositoryMock.configurarSecuenciaTurno(secuenciaTurno))
                .thenReturn(Mono.just(Boolean.TRUE));

        Mono<Boolean> seConfigurarSecuenciaTurno = personalUseCaseMock
                .configurarSecuenciaTurno(secuenciaTurno);

        StepVerifier.create(seConfigurarSecuenciaTurno)
                .expectNext(Boolean.TRUE)
                .expectComplete()
                .verify();
    }
}
