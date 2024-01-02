package co.com.sura.maestros;

import co.com.sura.maestro.CrudMaestroUseCase;
import co.com.sura.maestros.entity.*;
import co.com.sura.maestros.gateway.MaestroRepository;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.Mockito;
import org.mockito.junit.jupiter.MockitoExtension;
import reactor.core.publisher.Flux;
import reactor.core.publisher.Mono;
import reactor.test.StepVerifier;

import java.util.ArrayList;
import java.util.List;


@ExtendWith(MockitoExtension.class)
class MaestrosUseCaseTest {

    @Mock
    private MaestroRepository maestroRepositoryMock;

    @InjectMocks
    private CrudMaestroUseCase crudMaestroUseCase;


    @Test
    void consultarRegionales(){
        List<Regional> regionales = new ArrayList<>();
        regionales.add(Regional.builder().build());
        regionales.add(Regional.builder().build());

        Mockito.when(maestroRepositoryMock.consultarRegional()).thenReturn(Flux.fromIterable(regionales));

        Flux<Regional> consultaRegionales = crudMaestroUseCase.consultarRegionales();

        StepVerifier.create(consultaRegionales)
                .expectNextCount(2)
                .expectComplete()
                .verify();
    }

    @Test
    void consultarRegionalesById(){
        String idRegional = "427";
        Regional regional = Regional.builder().id(idRegional).build();

        Mockito.when(maestroRepositoryMock.consultarRegionalById(idRegional)).thenReturn(Mono.just(regional));

        Mono<Regional> consultaRegional = crudMaestroUseCase.consultarRegionalById(idRegional);

        StepVerifier.create(consultaRegional)
                .expectNext(regional)
                .expectComplete()
                .verify();
    }

    @Test
    void consultarHorariosTurno(){
        List<HorarioTurno> horarioTurnos = new ArrayList<>();
        horarioTurnos.add(HorarioTurno.builder().build());
        horarioTurnos.add(HorarioTurno.builder().build());

        Mockito.when(maestroRepositoryMock.consultarHorarioTurno()).thenReturn(Flux.fromIterable(horarioTurnos));

        Flux<HorarioTurno> consultaHorariosTurno = crudMaestroUseCase.consultarHorarioTurno();

        StepVerifier.create(consultaHorariosTurno)
                .expectNextCount(2)
                .expectComplete()
                .verify();
    }

    @Test
    void consultarHorariosTurnoById(){
        Integer idHorarioTurno = 1;
        HorarioTurno horarioTurno = HorarioTurno.builder().id(idHorarioTurno).build();

        Mockito.when(maestroRepositoryMock.consultarHorarioTurnoById(idHorarioTurno)).thenReturn(Mono.just(horarioTurno));

        Mono<HorarioTurno> consultaHorario = crudMaestroUseCase.consultarHorarioTurnoById(idHorarioTurno);

        StepVerifier.create(consultaHorario)
                .expectNext(horarioTurno)
                .expectComplete()
                .verify();
    }

    @Test
    void consultarTipoIdentificacion(){
        List<TipoIdentificacion> tipoIdentificacion = new ArrayList<>();
        tipoIdentificacion.add(TipoIdentificacion.builder().build());
        tipoIdentificacion.add(TipoIdentificacion.builder().build());

        Mockito.when(maestroRepositoryMock.consultarTipoIdentificacion()).thenReturn(Flux.fromIterable(tipoIdentificacion));

        Flux<TipoIdentificacion> consultaTipoIdentificacion = crudMaestroUseCase.consultarTipoIdentificacion();

        StepVerifier.create(consultaTipoIdentificacion)
                .expectNextCount(2)
                .expectComplete()
                .verify();
    }

    @Test
    void consultarTipoIdentificacionById(){
        Integer idTipoIdentificacion = 1;
        TipoIdentificacion tipoIdentificacion = TipoIdentificacion.builder().id(idTipoIdentificacion).build();

        Mockito.when(maestroRepositoryMock.consultarTipoIdentificacionById(idTipoIdentificacion))
                .thenReturn(Mono.just(tipoIdentificacion));

        Mono<TipoIdentificacion> consultaTipoIdentificacion = crudMaestroUseCase
                .consultarTipoIdentificacionById(idTipoIdentificacion);

        StepVerifier.create(consultaTipoIdentificacion)
                .expectNext(tipoIdentificacion)
                .expectComplete()
                .verify();
    }

    @Test
    void consultarEstadoCita(){
        List<EstadoCita> estadoCitas = new ArrayList<>();
        estadoCitas.add(EstadoCita.builder().build());
        estadoCitas.add(EstadoCita.builder().build());

        Mockito.when(maestroRepositoryMock.consultarEstadosCita()).thenReturn(Flux.fromIterable(estadoCitas));

        Flux<EstadoCita> consultaEstadosCita = crudMaestroUseCase.consultarEstadosCita();

        StepVerifier.create(consultaEstadosCita)
                .expectNextCount(2)
                .expectComplete()
                .verify();
    }
    @Test
    void consultarProfesion(){
        List<Profesion> profesiones = new ArrayList<>();
        profesiones.add(Profesion.builder().build());
        profesiones.add(Profesion.builder().build());

        Mockito.when(maestroRepositoryMock.consultarProfesiones()).thenReturn(Flux.fromIterable(profesiones));

        Flux<Profesion> consultaConsultarProfesiones = crudMaestroUseCase.consultarProfesiones();

        StepVerifier.create(consultaConsultarProfesiones)
                .expectNextCount(2)
                .expectComplete()
                .verify();
    }
    @Test
    void consultarMotivosCancelacionCita(){
        List<MotivoCancelacionCita> motivoCancelacionCitas = new ArrayList<>();
        motivoCancelacionCitas.add(MotivoCancelacionCita.builder().build());
        motivoCancelacionCitas.add(MotivoCancelacionCita.builder().build());

        Mockito.when(maestroRepositoryMock.consultarMotivosCancelacionCita())
                .thenReturn(Flux.fromIterable(motivoCancelacionCitas));

        Flux<MotivoCancelacionCita> motivosCancelacionCita = crudMaestroUseCase.consultarMotivosCancelacionCita();

        StepVerifier.create(motivosCancelacionCita)
                .expectNextCount(2)
                .expectComplete()
                .verify();
    }
}