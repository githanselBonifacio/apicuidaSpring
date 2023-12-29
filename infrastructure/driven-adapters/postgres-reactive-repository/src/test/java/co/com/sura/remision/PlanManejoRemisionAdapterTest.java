package co.com.sura.remision;

import co.com.sura.postgres.agenda.data.CitaData;
import co.com.sura.postgres.agenda.repository.CitaRepository;
import co.com.sura.postgres.maestros.data.HorarioTurnoData;
import co.com.sura.postgres.maestros.repository.HorarioTurnoRepository;
import co.com.sura.postgres.remision.adapter.ConverterRemision;
import co.com.sura.postgres.remision.adapter.PlanManejoRemisionAdapter;
import co.com.sura.postgres.remision.data.procedimientos.*;
import co.com.sura.postgres.remision.data.tratamientos.TratamientoData;
import co.com.sura.postgres.remision.repository.procedimientos.*;
import co.com.sura.postgres.remision.repository.tratamientos.TratamientoRepository;
import co.com.sura.remision.dto.*;
import co.com.sura.remision.entity.datosremision.Medicamento;
import co.com.sura.remision.entity.procedimientos.*;
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
import java.time.LocalDateTime;
import java.time.LocalTime;
import java.util.ArrayList;
import java.util.List;

@ExtendWith(MockitoExtension.class)
class PlanManejoRemisionAdapterTest {

    private final String idRemision = "asdf45";
    @Mock
    private  HorarioTurnoRepository horarioTurnoRepositoryMock;
    @Mock
    private  CitaRepository citaRepositoryMock;
    @Mock
    private  TratamientoRepository tratamientoRepositoryMock;
    @Mock
    private  CanalizacionRepository canalizacionRepositoryMock;
    @Mock
    private  SecrecionRepository secrecionRepositoryMock;
    @Mock
    private  CuracionRepository curacionRepositoryMock;
    @Mock
    private  FototerapiaRepository fototerapiaRepositoryMock;
    @Mock
    private  SondajeRepository sondajeRepositoryMock;
    @Mock
    private  TomaMuestraRepository tomaMuestraRepositoryMock;
    @Mock
    private  SoporteNutricionalRepository soporteNutricionalRepositoryMock;

    @InjectMocks
    private PlanManejoRemisionAdapter planManejoRemisionAdapter;


    @Test
    void registrarPlanManejo(){
        LocalDateTime fechaTest1 =LocalDateTime.of(LocalDate.now(),LocalTime.of(7,0));
        String idRegional = "427";
        Integer idHorario = 1;
        Integer idEstado = 1;
        Integer duracion = 900;
        Integer holura = 900;
        String especialidad = "especialidad";
        //Data request
        RemisionTestData remisionTestData = new RemisionTestData();
        RemisionRequest remisionRequest = remisionTestData.remisionRequest;
        remisionRequest.setIdRemision(idRemision);

        //tratamientos
       List<TratamientoRequest> tratamientosRequest =  new ArrayList<>(){{add(TratamientoRequest
                .builder()
                .tipoTratamiento(TipoTratamientoRequest
                        .builder()
                        .nombre("Medicamento")
                        .build())
                .medicamento(Medicamento.builder()
                        .nombre("medicamento")
                        .codigoMedicamento("codigo")
                        .presentacion("presentacion")
                        .build())
                .unidadDosis(UnidadDosisRequest.builder()
                        .descripcion("descripcion unidad dosis")
                        .build())
                .viaAdministracion(ViaAdministracionRequest.builder()
                        .descripcion("descripcion via administracion")
                        .build())
                .frecuencia(FrecuenciaRequest.builder()
                        .descripcion("descripcion frecuencia")
                        .build())
                .tipoPrestacion(TipoPrestacionRequest.builder()
                        .tipoPrestacion("prestacion")
                        .build())
                .build());}};
        //procedimientos
        List<Canalizacion> canalizaciones = new ArrayList<>(){{add(Canalizacion.builder().build());}};
        List<Fototerapia> fototerapias = new ArrayList<>(){{add(Fototerapia.builder().build());}};
        List<SecrecionRequest> secrecionRequest = new ArrayList<>(){{add(SecrecionRequest.builder().build());}};
        List<SondajeRequest> sondajeRequest = new ArrayList<>(){{add(SondajeRequest.builder().build());}};
        List<SoporteNutricionalRequest> soporteNutricionalRequest = new ArrayList<>(){{add(SoporteNutricionalRequest.builder()
                .medicamento(Medicamento.builder()
                        .nombre("medicamento")
                        .codigoMedicamento("codigo")
                        .presentacion("presentacion")
                        .build())
                .unidadDosis(UnidadDosisRequest.builder()
                        .descripcion("descripcion unidad dosis")
                        .build())
                .build());}};

        List<TomaMuestraRequest> tomaMuestrasRequest = new ArrayList<>(){{add(TomaMuestraRequest.builder()
                .tipoMuestra(TipoMuestra.builder()
                        .descripcion("tipo muestra")
                        .build())
                .build());}};

        List<CuracionRequest> curacionRequest = new ArrayList<>(){{add(CuracionRequest.builder()
                .tipoCuracion(TipoCuracionRequest.builder()
                        .idTipoCuracion("1")
                        .descripcion("mayor")
                        .build())
                .descripcion("curacion mayor")
                .build());}};
        //citas
        List<CitaRequest> citaRequests = new ArrayList<>();
        citaRequests.add(
                 CitaRequest.builder()
                         .idCita(remisionRequest.getIdRemision()+"-1")
                         .duracion(duracion)
                         .holgura(holura)
                         .fechaInicio(fechaTest1)
                         .especialidad(especialidad)
                         .tratamientos(tratamientosRequest)
                         .procedimientos(ProcedimientoRequest.builder()
                                 .canalizaciones(canalizaciones)
                                 .fototerapias(fototerapias)
                                 .secreciones(secrecionRequest)
                                 .sondajes(sondajeRequest)
                                 .soporteNutricionales(soporteNutricionalRequest)
                                 .tomaMuestras(tomaMuestrasRequest)
                                 .curaciones(curacionRequest)
                                 .build())
                         .build());

        //data
        List<CitaData> citasData = new ArrayList<>();
        CitaData citaData1 =   CitaData.builder()
                .idRemision(idRemision)
                .idCita(idRemision+"-1")
                .fechaInicio(fechaTest1)
                .fechaProgramada(fechaTest1)
                .idRegional(idRegional)
                .idEstado(idEstado)
                .duracion(duracion)
                .holgura(holura)
                .especialidad(especialidad)
                .latitud(0.0)
                .longitud(0.0)
                .build();

        CitaData citaData2 =   CitaData.builder()
                .idRemision(idRemision)
                .idCita(idRemision+"-1")
                .fechaInicio(fechaTest1)
                .fechaProgramada(fechaTest1)
                .idRegional(idRegional)
                .idHorarioTurno(idHorario)
                .idEstado(idEstado)
                .duracion(duracion)
                .holgura(holura)
                .especialidad(especialidad)
                .latitud(0.0)
                .longitud(0.0)
                .build();
        citasData.add(citaData1);
        List<HorarioTurnoData> horarioTurnoDataList = new ArrayList<>();
        horarioTurnoDataList.add(
                HorarioTurnoData.builder()
                        .id(idHorario)
                        .horaInicio(LocalTime.of(6,0))
                        .horaFin(LocalTime.of(13,59))
                        .esHorarioBase(Boolean.TRUE)
                        .build()
        );
        //mocks
      Mockito.when(horarioTurnoRepositoryMock.findAll())
                .thenReturn(Flux.fromIterable(horarioTurnoDataList));

        Mockito.when(citaRepositoryMock.insertMultiplescitas(citasData))
                .thenReturn(Mono.empty());

        Mockito.when(citaRepositoryMock.save(citaData2))
                .thenReturn(Mono.just(citaData2));

        Mockito.when(citaRepositoryMock.insertMultiplescitas(citasData))
                .thenReturn(Mono.empty());

        //mocks plan de manejo
        List<TratamientoData> tratamientosData = ConverterRemision.extraerTratamientoData(citaRequests);

        Mockito.when(tratamientoRepositoryMock.saveAll(tratamientosData))
                .thenReturn(Flux.fromIterable(tratamientosData));

        List<CanalizacionData> canalizacionesData = ConverterRemision.extraerCanalizacionData(citaRequests);
        Mockito.when(canalizacionRepositoryMock.saveAll(canalizacionesData))
                .thenReturn(Flux.fromIterable(canalizacionesData));

        List<FototerapiaData> fototerapiaData = ConverterRemision.extraerFototerapiaData(citaRequests);
        Mockito.when(fototerapiaRepositoryMock.saveAll(fototerapiaData))
                .thenReturn(Flux.fromIterable(fototerapiaData));

        List<SecrecionData> secrecionData = ConverterRemision.extraerSecrecionData(citaRequests);
        Mockito.when(secrecionRepositoryMock.saveAll(secrecionData))
                .thenReturn(Flux.fromIterable(secrecionData));

        List<SondajeData> sondajeData = ConverterRemision.extraerSondajeData(citaRequests);
        Mockito.when(sondajeRepositoryMock.saveAll(sondajeData))
                .thenReturn(Flux.fromIterable(sondajeData));

        List<SoporteNutricionalData> soporteNutricionalData = ConverterRemision.extraerSoporteNutricionalData(citaRequests);
        Mockito.when(soporteNutricionalRepositoryMock.saveAll(soporteNutricionalData))
                .thenReturn(Flux.fromIterable(soporteNutricionalData));

        List<TomaMuestraData> tomaMuestraData = ConverterRemision.extraerTomaMuestraData(citaRequests);
        Mockito.when(tomaMuestraRepositoryMock.saveAll(tomaMuestraData))
                .thenReturn(Flux.fromIterable(tomaMuestraData));

        List<CuracionData> curacionData = ConverterRemision.extraerCuracionData(citaRequests);
        Mockito.when(curacionRepositoryMock.saveAll(curacionData))
                .thenReturn(Flux.fromIterable(curacionData));

        Mono<Void> response = planManejoRemisionAdapter.registrarPlanManejo(remisionRequest,citaRequests,0);
       StepVerifier.create(response)
                .expectComplete()
                .verify();
    }

    @Test
    void eliminarPlanManejoByidRemision(){

        Mockito.when(soporteNutricionalRepositoryMock.deleteByIDRemision(idRemision))
                .thenReturn(Mono.empty());

        Mockito.when(tomaMuestraRepositoryMock.deleteByIDRemision(idRemision))
                .thenReturn(Mono.empty());

        Mockito.when(sondajeRepositoryMock.deleteByIDRemision(idRemision))
                .thenReturn(Mono.empty());

        Mockito.when(fototerapiaRepositoryMock.deleteByIDRemision(idRemision))
                .thenReturn(Mono.empty());

        Mockito.when(curacionRepositoryMock.deleteByIDRemision(idRemision))
                .thenReturn(Mono.empty());

        Mockito.when(secrecionRepositoryMock.deleteByIDRemision(idRemision))
                .thenReturn(Mono.empty());

        Mockito.when(canalizacionRepositoryMock.deleteByIDRemision(idRemision))
                .thenReturn(Mono.empty());

        Mockito.when(tratamientoRepositoryMock.deleteByIDRemision(idRemision))
                .thenReturn(Mono.empty());
        Mono<Void> response  = planManejoRemisionAdapter.eliminarPlanManejoByidRemision(idRemision);

        StepVerifier.create(response)
                .expectComplete()
                .verify();
    }
}