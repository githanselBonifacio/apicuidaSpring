package co.com.sura.maestros;


import co.com.sura.maestros.entity.*;
import co.com.sura.maestros.gateway.MaestroRepository;
import co.com.sura.postgres.maestros.adapter.MaestroRepositoryAdapter;
import co.com.sura.postgres.maestros.data.*;
import co.com.sura.postgres.maestros.repository.*;
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
 class MaestrosRepositoryAdapterTest {

    @Mock
    private MaestroRepository maestroRepositoryMock;
    @Mock
    private  RegionalesRepository regionalesRepositoryMock;
    @Mock
    private  HorarioTurnoRepository horarioTurnoRepositoryMock;
    @Mock
    private  TipoIdentificacionRepository tipoIdentificacionRepositoryMock;
    @Mock
    private  EstadoCitaRepository estadoCitaRepositoryMock;
    @Mock
    private  ProfesionRepository profesionRepositoryMock;

    @Mock MotivoCancelacionCitaRepository motivoCancelacionCitaRepositoryMock;

    @InjectMocks
    private MaestroRepositoryAdapter maestroRepositoryAdapter;

    @Test
    void consultarRegional(){

        List<RegionalData> regionales = new ArrayList<>();
        regionales.add(RegionalData.builder()
                        .id("427")
                .build());

        Mockito.when(regionalesRepositoryMock.findAll())
                .thenReturn(Flux.fromIterable(regionales));

        Flux<Regional> response = maestroRepositoryAdapter.consultarRegional();


        StepVerifier.create(response)
                .expectNextCount(1)
                .expectComplete()
                .verify();

    }
    @Test
    void consultarRegionalById(){
        String idRegional = "427";
        RegionalData regional = RegionalData.builder()
                .id(idRegional)
                .build();

        Mockito.when(regionalesRepositoryMock.findById(idRegional))
                .thenReturn(Mono.just(regional));

        Mono<Regional> response = maestroRepositoryAdapter.consultarRegionalById(idRegional);


        StepVerifier.create(response)
                .expectNextMatches(r ->r.getId().equals(idRegional))
                .expectComplete()
                .verify();

    }

    @Test
    void consultarHorarioTurno(){
        Integer idHorarioTurno = 1;
        List<HorarioTurnoData> horariosTurnoData = new ArrayList<>();
        horariosTurnoData.add(HorarioTurnoData.builder()
                .id(idHorarioTurno)
                .build());

        Mockito.when(horarioTurnoRepositoryMock.findAll())
                .thenReturn(Flux.fromIterable(horariosTurnoData));

        Flux<HorarioTurno> response = maestroRepositoryAdapter.consultarHorarioTurno();


        StepVerifier.create(response)
                .expectNextCount(1)
                .expectComplete()
                .verify();

    }
    @Test
    void consultarHorarioTurnoById(){
        Integer idHorarioTurno = 1;
        HorarioTurnoData horarioTurno = HorarioTurnoData.builder()
                .id(idHorarioTurno)
                .build();

        Mockito.when(horarioTurnoRepositoryMock.findById(idHorarioTurno))
                .thenReturn(Mono.just(horarioTurno));

        Mono<HorarioTurno> response = maestroRepositoryAdapter.consultarHorarioTurnoById(idHorarioTurno);


        StepVerifier.create(response)
                .expectNextMatches(h ->h.getId().equals(idHorarioTurno))
                .expectComplete()
                .verify();

    }


    @Test
    void consultarTipoIdentificacion(){
        Integer idtipoIdentificacion = 1;
        List<TipoIdentificacionData> tiposIdentificacionData = new ArrayList<>();
        tiposIdentificacionData.add(TipoIdentificacionData.builder()
                .id(idtipoIdentificacion)
                .build());

        Mockito.when(tipoIdentificacionRepositoryMock.findAll())
                .thenReturn(Flux.fromIterable(tiposIdentificacionData));

        Flux<TipoIdentificacion> response = maestroRepositoryAdapter.consultarTipoIdentificacion();


        StepVerifier.create(response)
                .expectNextCount(1)
                .expectComplete()
                .verify();

    }
    @Test
    void consultarTipoIdentificacionById(){
        Integer idTipoIdentificacion = 1;
        TipoIdentificacionData tipoIdentificacionData = TipoIdentificacionData.builder()
                .id(idTipoIdentificacion)
                .build();

        Mockito.when(tipoIdentificacionRepositoryMock.findById(idTipoIdentificacion))
                .thenReturn(Mono.just(tipoIdentificacionData));

        Mono<TipoIdentificacion> response = maestroRepositoryAdapter.consultarTipoIdentificacionById(idTipoIdentificacion);


        StepVerifier.create(response)
                .expectNextMatches(i ->i.getId().equals(idTipoIdentificacion))
                .expectComplete()
                .verify();

    }
    @Test
    void consultarEstadosCita(){
        String idEstadoCita = "1";
        List<EstadoCitaData> estadosCitaData = new ArrayList<>();
        estadosCitaData.add(EstadoCitaData.builder()
                .id(idEstadoCita)
                .build());

        Mockito.when(estadoCitaRepositoryMock.findAll())
                .thenReturn(Flux.fromIterable(estadosCitaData));

        Flux<EstadoCita> response = maestroRepositoryAdapter.consultarEstadosCita();


        StepVerifier.create(response)
                .expectNextCount(1)
                .expectComplete()
                .verify();

    }

    @Test
    void consultarProfesiones(){
        Integer idProfesion = 1;
        List<ProfesionData> profesionesData = new ArrayList<>();
        profesionesData.add(ProfesionData.builder()
                .idProfesion(idProfesion)
                .build());

        Mockito.when(profesionRepositoryMock.findAll())
                .thenReturn(Flux.fromIterable(profesionesData));

        Flux<Profesion> response = maestroRepositoryAdapter.consultarProfesiones();


        StepVerifier.create(response)
                .expectNextCount(1)
                .expectComplete()
                .verify();

    }
    @Test
    void consultarMotivoCancelacionCita(){
        List<MotivoCancelacionCitaData> motivoCancelacionCitaData = new ArrayList<>();
        motivoCancelacionCitaData.add(MotivoCancelacionCitaData.builder()
                .build());

        Mockito.when(motivoCancelacionCitaRepositoryMock.findAll())
                .thenReturn(Flux.fromIterable(motivoCancelacionCitaData));

        Flux<MotivoCancelacionCita> response = maestroRepositoryAdapter.consultarMotivosCancelacionCita();


        StepVerifier.create(response)
                .expectNextCount(1)
                .expectComplete()
                .verify();

    }
}
