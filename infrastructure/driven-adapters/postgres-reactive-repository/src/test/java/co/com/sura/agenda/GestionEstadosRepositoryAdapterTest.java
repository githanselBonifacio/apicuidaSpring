package co.com.sura.agenda;

import co.com.sura.autoagendador.models.AutoAgendador;
import co.com.sura.constantes.Mensajes;
import co.com.sura.genericos.EstadosCita;
import co.com.sura.mapbox.gateway.MapboxServiceRepository;
import co.com.sura.postgres.agenda.adapter.AgendamientoAutomaticoAdapter;
import co.com.sura.postgres.agenda.adapter.GestionEstadosCitaAdapter;
import co.com.sura.postgres.agenda.data.CitaData;
import co.com.sura.postgres.agenda.repository.CitaRepository;
import co.com.sura.postgres.maestros.data.HorarioTurnoData;
import co.com.sura.postgres.maestros.repository.HorarioTurnoRepository;
import co.com.sura.postgres.maestros.repository.RegionalesRepository;
import co.com.sura.postgres.moviles.data.DesplazamientoData;
import co.com.sura.postgres.moviles.data.DesplazamientoRepository;
import co.com.sura.postgres.personal.repository.ProfesionalRepository;
import co.com.sura.postgres.reportes.repository.RegistroCancelacionCitaRepository;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.Mock;
import org.mockito.Mockito;
import org.mockito.junit.jupiter.MockitoExtension;
import reactor.core.publisher.Mono;
import reactor.test.StepVerifier;
import java.time.LocalDate;
import java.time.LocalDateTime;
import java.time.LocalTime;

import static org.mockito.Mockito.when;

@ExtendWith(MockitoExtension.class)
 class GestionEstadosRepositoryAdapterTest {

    private final String idCita = "dsf5df-1";
    private final Integer idHorarioTurno = 1;
    private final String idRegional = "427";
    private  final LocalDateTime fechaProgramada = LocalDateTime.of(LocalDate.now(), LocalTime.of(8,30));
    private  CitaData citaData;
    @Mock(lenient = true)
    private  CitaRepository citaRepositoryMock;
    @Mock
    private  DesplazamientoRepository desplazamientoRepositoryMock;
    @Mock
    private  HorarioTurnoRepository horarioTurnoRepositoryMock;
    @Mock
    private RegistroCancelacionCitaRepository registroCancelacionCitaRepositoryMock;

    private GestionEstadosCitaAdapter gestionEstadosCitaAdapter;

    @BeforeEach
    void setUp(){
        AgendamientoAutomaticoAdapter agendamientoAutomaticoAdapterMock = new AgendamientoAutomaticoAdapter(
                citaRepositoryMock,
                Mockito.mock(ProfesionalRepository.class),
                desplazamientoRepositoryMock,
                Mockito.mock(RegionalesRepository.class),
                horarioTurnoRepositoryMock,
                Mockito.mock(MapboxServiceRepository.class),
                Mockito.mock(AutoAgendador.class)
        );
        gestionEstadosCitaAdapter = new GestionEstadosCitaAdapter(
                citaRepositoryMock,
                agendamientoAutomaticoAdapterMock,
                desplazamientoRepositoryMock,
                horarioTurnoRepositoryMock,
                registroCancelacionCitaRepositoryMock);
        this.citaData = CitaData.builder()
                .idCita(idCita)
                .fechaProgramada(fechaProgramada)
                .fechaInicio(fechaProgramada)
                .idRegional(idRegional)
                .idHorarioTurno(idHorarioTurno)
                .duracion(600)
                .holgura(600)
                .build();
    }

    @Test
    void agendarToProfesionalFechaOcupada (){

        //mocks citas
        when(citaRepositoryMock.findById(idCita))
                .thenReturn(Mono.just(this.citaData));

        String idProfesional = "9898989";
        when(citaRepositoryMock.findMasCercanaAnterior(
                        fechaProgramada,idCita,idHorarioTurno,idRegional, idProfesional))
                .thenReturn(Mono.just(this.citaData));

        when(citaRepositoryMock.findMasCercanaPosterior(
                        fechaProgramada
                        ,idCita,idHorarioTurno,idRegional, idProfesional))
                .thenReturn(Mono.just(citaData));

        //mocks desplazamientos
        when(desplazamientoRepositoryMock.findByIdCitaPartida(idCita))
                .thenReturn(Mono.just(DesplazamientoData.builder().duracion(600).build()));

        when(desplazamientoRepositoryMock.findBySede(
                        fechaProgramada,idRegional, idProfesional,idHorarioTurno))
                .thenReturn(Mono.just(DesplazamientoData.builder().duracion(600).build()));


        //mocks horario turno
        when(horarioTurnoRepositoryMock.findById(idHorarioTurno))
                .thenReturn(Mono.just(HorarioTurnoData.builder()
                                .id(idHorarioTurno)
                                .horaInicio(LocalTime.of(6,0))
                                .horaFin(LocalTime.of(13,59))
                                .esHorarioBase(Boolean.TRUE)
                                .build()));


        Mono<Boolean> response = gestionEstadosCitaAdapter.agendarToProfesional(
                idCita, idProfesional,fechaProgramada, idHorarioTurno,idRegional);

        StepVerifier.create(response)
                .expectErrorMessage(Mensajes.ERROR_FECHA_CITA)
                .verify();
    }

  @Test
    void confirmarCita(){

      //mocks citas
      when(citaRepositoryMock.findById(idCita))
              .thenReturn(Mono.just(citaData.toBuilder()
                      .idEstado(EstadosCita.AGENDADA.getEstado())
                      .build()));

      when(citaRepositoryMock.updateEstado(idCita,EstadosCita.CONFIRMADA.getEstado()))
              .thenReturn(Mono.empty());

      Mono<Boolean> response = gestionEstadosCitaAdapter.confirmarCita(idCita);

      StepVerifier.create(response)
              .expectNext(Boolean.TRUE)
              .verifyComplete();
  }

    @Test
    void iniciarAtencionCita(){

        //mocks citas
        when(citaRepositoryMock.findById(idCita))
                .thenReturn(Mono.just(this.citaData.toBuilder()
                        .idEstado(EstadosCita.CONFIRMADA.getEstado())
                        .build()));

        when(citaRepositoryMock.updateEstado(idCita,EstadosCita.EN_PROGRESO.getEstado()))
                .thenReturn(Mono.empty());

        Mono<Boolean> response = gestionEstadosCitaAdapter.iniciarAtencionCita(idCita);

        StepVerifier.create(response)
                .expectNext(Boolean.TRUE)
                .verifyComplete();
    }

    @Test
    void finalizarAtencionCita(){

        //mocks citas
        when(citaRepositoryMock.findById(idCita))
                .thenReturn(Mono.just(this.citaData.toBuilder()
                        .idEstado(EstadosCita.EN_PROGRESO.getEstado())
                        .build()));

        when(citaRepositoryMock.updateEstado(idCita,EstadosCita.FINALIZADA.getEstado()))
                .thenReturn(Mono.empty());

        Mono<Boolean> response = gestionEstadosCitaAdapter.finalizarAtencionCita(idCita);

        StepVerifier.create(response)
                .expectNext(Boolean.TRUE)
                .verifyComplete();
    }
    @Test
    void estadoNoValido(){

        //mocks citas
        when(citaRepositoryMock.findById(idCita))
                .thenReturn(Mono.just(this.citaData.toBuilder()
                        .idEstado(EstadosCita.SIN_AGENDAR.getEstado())
                        .build()));

        when(citaRepositoryMock.updateEstado(idCita,EstadosCita.FINALIZADA.getEstado()))
                .thenReturn(Mono.empty());

        Mono<Boolean> response = gestionEstadosCitaAdapter.finalizarAtencionCita(idCita);

        StepVerifier.create(response)
                .expectErrorMessage(Mensajes.ERROR_ESTADO_CITA+ EstadosCita.getNombreEstado(EstadosCita.EN_PROGRESO))
                .verify();

    }

}
