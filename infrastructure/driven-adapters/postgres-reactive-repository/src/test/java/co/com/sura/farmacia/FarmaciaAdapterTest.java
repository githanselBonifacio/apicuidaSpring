package co.com.sura.farmacia;

import co.com.sura.agenda.entity.PacienteTratamientoCita;
import co.com.sura.genericos.EstadosCita;
import co.com.sura.postgres.farmacia.adapter.FarmaciaAdapter;
import co.com.sura.postgres.remision.repository.datospaciente.PacienteRepository;
import co.com.sura.postgres.remision.repository.procedimientos.SoporteNutricionalRepository;
import co.com.sura.postgres.remision.repository.tratamientos.TratamientoRepository;
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
import java.util.ArrayList;
import java.util.List;

@ExtendWith(MockitoExtension.class)
 class FarmaciaAdapterTest {

    @Mock
    private  PacienteRepository pacienteRepositoryMock;
    @Mock
    private  SoporteNutricionalRepository soporteNutricionalRepositoryMock;
    @Mock
    private  TratamientoRepository tratamientoRepositoryMock;
    @InjectMocks
    private FarmaciaAdapter farmaciaAdapter;

    @Test
    void consultarAllPacienteWithMedicamentosToFarmacia(){
        String numeroIdentificacion = "989898";
        List<PacienteTratamientoCita> pacienteTratamientoCitas = new ArrayList<>();
        pacienteTratamientoCitas.add(PacienteTratamientoCita.builder()
                .numeroIdentificacion(numeroIdentificacion)
                        .fechaProgramada(LocalDateTime.now())
                .notificado(Boolean.FALSE)
                .build());

        Mockito.when(pacienteRepositoryMock.findAllTratamientosPacientes(
             EstadosCita.CONFIRMADA.getEstado(),EstadosCita.EN_PROGRESO.getEstado(),EstadosCita.FINALIZADA.getEstado()))
          .thenReturn(Flux.fromIterable(pacienteTratamientoCitas));

        Mockito.when(pacienteRepositoryMock.findAllSoporteNutricionalPacientes(
            EstadosCita.CONFIRMADA.getEstado(),EstadosCita.EN_PROGRESO.getEstado(),EstadosCita.FINALIZADA.getEstado()))
                .thenReturn(Flux.fromIterable(pacienteTratamientoCitas));

        Flux<PacienteTratamientoCita> response = farmaciaAdapter.consultarAllPacienteWithMedicamentosToFarmacia();


        StepVerifier.create(response)
                .expectNextCount(1)
                .expectNext(pacienteTratamientoCitas.get(0))
                .expectComplete()
                .verify();
    }
    @Test
    void consultarAllPacienteWithMedicamentosToFarmaciaByFilter(){
        String numeroIdentificacion = "989898";
        LocalDate fecha = LocalDate.now();
        String idRegional = "427";
        Integer idHorarioTurno = 1;
        List<PacienteTratamientoCita> pacienteTratamientoCitas = new ArrayList<>();
        pacienteTratamientoCitas.add(PacienteTratamientoCita.builder()
                .numeroIdentificacion(numeroIdentificacion)
                .fechaProgramada(LocalDateTime.now())
                .notificado(Boolean.FALSE)
                .build());

        Mockito.when(pacienteRepositoryMock.findAllTratamientosPacientesByTurnoRegionalHorario(
                fecha,idRegional,idHorarioTurno,
                EstadosCita.CONFIRMADA.getEstado(),
                EstadosCita.EN_PROGRESO.getEstado(),
                EstadosCita.FINALIZADA.getEstado()))
                .thenReturn(Flux.fromIterable(pacienteTratamientoCitas));

        Mockito.when(pacienteRepositoryMock.findAllSoporteNutricionalPacientesByTurnoRegionalHorario(
                fecha,idRegional,idHorarioTurno,
                EstadosCita.CONFIRMADA.getEstado(),
                EstadosCita.EN_PROGRESO.getEstado(),
                EstadosCita.FINALIZADA.getEstado()))
                .thenReturn(Flux.fromIterable(pacienteTratamientoCitas));

        Flux<PacienteTratamientoCita> response = farmaciaAdapter
                .consultarAllPacienteWithMedicamentosToFarmaciaByFilter( fecha,idRegional,idHorarioTurno);


        StepVerifier.create(response)
                .expectNextCount(1)
                .expectNext(pacienteTratamientoCitas.get(0))
                .expectComplete()
                .verify();
    }

    @Test
    void notificarMedicamentosToFarmacia(){
        List<PacienteTratamientoCita> pacienteTratamientoCitas = new ArrayList<>();
        pacienteTratamientoCitas.add(PacienteTratamientoCita.builder()
                 .idTratamiento(1)
                .fechaProgramada(LocalDateTime.now())
                .notificado(Boolean.FALSE)
                .build());

        pacienteTratamientoCitas.add(PacienteTratamientoCita.builder()
                .idSoporteNutricional(1)
                .fechaProgramada(LocalDateTime.now())
                .notificado(Boolean.FALSE)
                .build());

        Mockito.when(tratamientoRepositoryMock.updateNotificar(pacienteTratamientoCitas.get(0)
                        .getIdTratamiento()))
                .thenReturn(Mono.empty());
        Mockito.when(soporteNutricionalRepositoryMock.updateNotificar(pacienteTratamientoCitas.get(1)
                        .getIdSoporteNutricional()))
                .thenReturn(Mono.empty());

        Mono<Boolean> response = farmaciaAdapter.notificarMedicamentosToFarmacia(pacienteTratamientoCitas);

        StepVerifier.create(response)
                .expectNext(Boolean.TRUE)
                .verifyComplete();
    }
}
