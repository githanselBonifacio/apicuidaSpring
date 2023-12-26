package co.com.sura.agenda;

import co.com.sura.agenda.entity.Cita;
import co.com.sura.autoagendador.models.AutoAgendador;
import co.com.sura.genericos.EstadosCita;
import co.com.sura.mapbox.entity.GeoUbicacion;
import co.com.sura.mapbox.gateway.MapboxServiceRepository;
import co.com.sura.moviles.entity.Desplazamiento;
import co.com.sura.postgres.agenda.adapter.AgendamientoAutomaticoAdapter;
import co.com.sura.postgres.agenda.data.CitaData;
import co.com.sura.postgres.agenda.repository.CitaRepository;
import co.com.sura.postgres.maestros.data.HorarioTurnoData;
import co.com.sura.postgres.maestros.data.RegionalData;
import co.com.sura.postgres.maestros.repository.HorarioTurnoRepository;
import co.com.sura.postgres.maestros.repository.RegionalesRepository;
import co.com.sura.postgres.moviles.data.DesplazamientoData;
import co.com.sura.postgres.moviles.data.DesplazamientoRepository;
import co.com.sura.postgres.personal.data.ProfesionalData;
import co.com.sura.postgres.personal.repository.ProfesionalRepository;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.Mock;
import org.mockito.Mockito;
import org.mockito.Spy;
import org.mockito.junit.jupiter.MockitoExtension;
import reactor.core.publisher.Flux;
import reactor.core.publisher.Mono;
import reactor.test.StepVerifier;
import java.time.LocalDate;
import java.time.LocalDateTime;
import java.time.LocalTime;
import java.util.ArrayList;
import java.util.List;

import static co.com.sura.genericos.EstadosCita.AGENDADA;

@ExtendWith(MockitoExtension.class)
class AgendamientoAutomaticoAdapterTest {
    private final LocalDateTime fechaProgramada = LocalDateTime.of(LocalDate.now(), LocalTime.of(7,30));
    private final Integer idHorarioTurno = 1;
    private final String idRegional = "427";
    private final String idProfesional = "878515";

    private final String idCita = "da6sf54-1";
    public static final Integer NUMERO_GENERACIONES           = 1;
    public static final Integer SIZE_POBLACION_INICIAL        = 2;
    public static final Integer NUMERO_PADRES_EMPAREJADOS     = 2;
    public static final Integer HOLGURA_DEFECTO               = 1200;
    public static final double  PENALIZACION_HOLGURA_NEGATIVA = 1e6;
    @Mock
    private  CitaRepository citaRepositoryMock;
    @Mock
    private  ProfesionalRepository profesionalRepositoryMock;
    @Mock
    private  DesplazamientoRepository desplazamientoRepositoryMock;
    @Mock
    private  RegionalesRepository regionalesRepositoryMock;
    @Mock
    private  HorarioTurnoRepository horarioTurnoRepositoryMock;

    @Mock
    private  MapboxServiceRepository mapboxServiceMock;

    @Spy
    private  AutoAgendador autoAgendadorMock;


    private AgendamientoAutomaticoAdapter agendamientoAutomaticoAdapter;

    @BeforeEach
    void setUp(){
        autoAgendadorMock = new AutoAgendador(NUMERO_GENERACIONES,
                SIZE_POBLACION_INICIAL,NUMERO_PADRES_EMPAREJADOS,PENALIZACION_HOLGURA_NEGATIVA,mapboxServiceMock);

        agendamientoAutomaticoAdapter = new AgendamientoAutomaticoAdapter(
                citaRepositoryMock,
                profesionalRepositoryMock,
                desplazamientoRepositoryMock,
                regionalesRepositoryMock,
                horarioTurnoRepositoryMock,
                mapboxServiceMock,
                autoAgendadorMock

        );
    }

    @Test
    void  consultarDesplazamientoByCitaPartida(){

        Mockito.when(desplazamientoRepositoryMock.findByFechaProgramada(
                fechaProgramada.toLocalDate(),idRegional,idHorarioTurno))
                .thenReturn(Flux.fromIterable(new ArrayList<>(){{Desplazamiento.builder().build();}}));

        Flux<Desplazamiento> response = agendamientoAutomaticoAdapter.consultarDesplazamientoByCitaPartida(
                fechaProgramada.toLocalDate(),idHorarioTurno,idRegional);

        StepVerifier.create(response)
                .expectNext(Desplazamiento.builder().build())
                .expectComplete();
    }

    @Test
    void desagendarTurnoCompleto(){

        Mockito.when(citaRepositoryMock.desagendarTurnoCompleto(
                        fechaProgramada.toLocalDate(),idHorarioTurno,idRegional, EstadosCita.SIN_AGENDAR.getEstado()))
                .thenReturn(Mono.empty());

        Mockito.when(desplazamientoRepositoryMock.deleteAllByFechaTurno(
                        fechaProgramada.toLocalDate(),idHorarioTurno,idRegional))
                .thenReturn(Mono.empty());

        Mono<Boolean> response = agendamientoAutomaticoAdapter.desagendarTurnoCompleto(
                fechaProgramada.toLocalDate(),idHorarioTurno,idRegional);

        StepVerifier.create(response)
                .expectNext(Boolean.TRUE)
                .expectComplete();
    }
    @Test
    void insertDesplazamientoCitaByProfesional(){

        GeoUbicacion geoInicial =   GeoUbicacion.builder().latitud(11.2).longitud(-7.2).build();
        GeoUbicacion geoSede =  GeoUbicacion.builder().latitud(11.8).longitud(-7.8).build();

        CitaData citaData = CitaData.builder()
                .idCita(idCita)
                .fechaProgramada(fechaProgramada)
                .fechaInicio(fechaProgramada)
                .duracion(900)
                .holgura(900)
                .latitud(geoInicial.getLatitud())
                .longitud(geoInicial.getLongitud())
                .build();

        List<CitaData> citas = new ArrayList<>();

        citas.add(citaData);

        DesplazamientoData desplazamientoData = DesplazamientoData.builder()
                .idCitaPartida("sede")
                .idCitaDestino(idCita)
                .fechaProgramada(fechaProgramada.withHour(6).withMinute(10))
                .idHorarioTurno(idHorarioTurno)
                .idRegional(idRegional)
                .tipo("DVISITA")
                .duracion(900)
                .holgura(1200)
                .idProfesional(idProfesional)
                .build();

        //mocks desplazamientos
        Mockito.when(desplazamientoRepositoryMock.deleteByFechaTurnoProfesional(
                        fechaProgramada.toLocalDate(),idHorarioTurno,idRegional,idProfesional))
                .thenReturn(Mono.empty());

        Mockito.when(desplazamientoRepositoryMock.save(desplazamientoData))
                .thenReturn(Mono.just(desplazamientoData));

        //mocks citas
        Mockito.when(citaRepositoryMock.findSedeByIdRegional(idRegional))
                .thenReturn(Mono.just(CitaData.builder()
                                .idCita("sede")
                                .fechaProgramada(fechaProgramada.withHour(6))
                                .duracion(600)
                                .longitud(geoSede.getLongitud())
                                .latitud(geoSede.getLatitud())
                                .build()));

        Mockito.when(citaRepositoryMock.findAllByTurnoRegionalProfesional(
                fechaProgramada.toLocalDate(),idHorarioTurno,idRegional,idProfesional,EstadosCita.CANCELADA.getEstado()))
                .thenReturn(Flux.fromIterable(citas));

        //mocks horario cita
        Mockito.when(horarioTurnoRepositoryMock.findById(idHorarioTurno))
                .thenReturn(Mono.just(HorarioTurnoData.builder()
                                .id(idHorarioTurno)
                                .horaInicio(LocalTime.of(6,0))
                                .horaFin(LocalTime.of(13,59))
                                .esHorarioBase(Boolean.TRUE)
                        .build()));

        //mocks mapboxService
        Mockito.when(mapboxServiceMock.calcularTiempoViaje(geoSede, geoInicial))
                .thenReturn(Mono.just(900));

        Mono<Boolean> response = agendamientoAutomaticoAdapter.insertDesplazamientoCitaByProfesional(
                fechaProgramada.toLocalDate(),idHorarioTurno,idRegional,idProfesional);

        StepVerifier.create(response)
                .expectNext(Boolean.TRUE)
                .verifyComplete();
    }
    @Test
    void  autoagendarTurnoCompleto(){
        //Data
        GeoUbicacion geoInicial =   GeoUbicacion.builder().latitud(11.2).longitud(-7.2).build();
        GeoUbicacion geoSede =  GeoUbicacion.builder().latitud(11.8).longitud(-7.8).build();

        //citas
        Cita cita = Cita.builder()
                .idCita(idCita)
                .fechaProgramada(fechaProgramada)
                .fechaInicio(fechaProgramada)
                .duracion(900)
                .holgura(900)
                .latitud(geoInicial.getLatitud())
                .longitud(geoInicial.getLongitud())
                .build();

        List<Cita> citas = new ArrayList<>();
        citas.add(cita);
        CitaData citaData = CitaData.builder()
                .idCita(idCita)
                .fechaProgramada(fechaProgramada)
                .fechaInicio(fechaProgramada)
                .duracion(900)
                .holgura(900)
                .latitud(geoInicial.getLatitud())
                .longitud(geoInicial.getLongitud())
                .build();

        List<CitaData> citasData = new ArrayList<>();
        citasData.add(citaData);

        //profesionales
        ProfesionalData profesionalData = ProfesionalData.builder()
                .numeroIdentificacion(idProfesional)
                .build();
        List<ProfesionalData> profesionalesData = new ArrayList<>();
        profesionalesData.add(profesionalData);

        //desplazamiento
        DesplazamientoData desplazamientoData= DesplazamientoData.builder()
                .idCitaPartida("sede-"+idRegional)
                .idCitaDestino(idCita)
                .fechaProgramada(fechaProgramada
                        .withHour(6)
                        .withMinute(10))
                .idHorarioTurno(idHorarioTurno)
                .idRegional(idRegional)
                .tipo("DVISITA")
                .holgura(HOLGURA_DEFECTO)
                .idProfesional(idProfesional)
                .duracion(900)
                .build();

        //mocks desplazamientos
        Mockito.when(desplazamientoRepositoryMock.deleteAllByFechaTurno(
                        fechaProgramada.toLocalDate(),idHorarioTurno,idRegional))
                .thenReturn(Mono.empty());

        Mockito.when(desplazamientoRepositoryMock.deleteByFechaTurnoProfesional(
                        fechaProgramada.toLocalDate(),idHorarioTurno,idRegional,idProfesional))
                .thenReturn(Mono.empty());

        Mockito.when(desplazamientoRepositoryMock.save(desplazamientoData))
                .thenReturn(Mono.just(desplazamientoData));

        //mocks citas
        Mockito.when(citaRepositoryMock.desagendarTurnoCompleto(
                        fechaProgramada.toLocalDate(),idHorarioTurno,idRegional,EstadosCita.SIN_AGENDAR.getEstado()))
                .thenReturn(Mono.empty());

        Mockito.when(citaRepositoryMock.findAllByTurnoRegionalHorario(
                        fechaProgramada.toLocalDate(),idHorarioTurno,idRegional,EstadosCita.CANCELADA.getEstado()))
                .thenReturn(Flux.fromIterable(citas));

        Mockito.when(citaRepositoryMock.updateEstadoAndProfesional(idCita,  AGENDADA.getEstado(),idProfesional))
                .thenReturn(Mono.empty());

        Mockito.when(citaRepositoryMock.findAllByTurnoRegionalProfesional(
             fechaProgramada.toLocalDate(),idHorarioTurno,idRegional,idProfesional,EstadosCita.CANCELADA.getEstado()))
                .thenReturn(Flux.fromIterable(citasData));

        Mockito.when(citaRepositoryMock.findSedeByIdRegional(idRegional))
                .thenReturn(Mono.just(CitaData.builder()
                                .idCita("sede-"+idRegional)
                                .idRegional(idRegional)
                                .fechaProgramada(fechaProgramada)
                                .duracion(600)
                                .latitud(geoSede.getLatitud())
                                .longitud(geoSede.getLongitud())
                                .build()));

        //profesionales
        Mockito.when(profesionalRepositoryMock.findFromTurnoRegional(
                        fechaProgramada.toLocalDate(),idRegional,idHorarioTurno))
                .thenReturn(Flux.fromIterable(profesionalesData));


        //maestros
        Mockito.when(regionalesRepositoryMock.findById(idRegional))
                .thenReturn(Mono.just(RegionalData.builder()
                                .id(idRegional)
                                .latitud(11.8)
                                .longitud(-7.8)
                                .build()));

        Mockito.when(horarioTurnoRepositoryMock.findById(idHorarioTurno))
                .thenReturn(Mono.just(HorarioTurnoData.builder()
                                .id(idHorarioTurno)
                                .horaInicio(LocalTime.of(6,0))
                                .horaFin(LocalTime.of(13,59))
                                .esHorarioBase(Boolean.TRUE)
                                .build()));

       Mockito.when(mapboxServiceMock.calcularTiempoViaje(geoSede, geoInicial))
                .thenReturn(Mono.just(900));

        //response
        Mono<Boolean> response = agendamientoAutomaticoAdapter.autoagendarTurnoCompleto(
                fechaProgramada.toLocalDate(),idHorarioTurno,idRegional);

        StepVerifier.create(response)
                .expectNext(Boolean.TRUE)
                .verifyComplete();
    }
}
