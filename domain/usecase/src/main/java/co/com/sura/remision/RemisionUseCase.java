package co.com.sura.remision;

import co.com.sura.dto.remision.CitaRequest;
import co.com.sura.dto.remision.NovedadRequest;
import co.com.sura.dto.remision.RemisionRequest;
import co.com.sura.entity.agenda.PacienteTratamientoCita;
import co.com.sura.entity.remision.*;
import reactor.core.publisher.Flux;
import reactor.core.publisher.Mono;

import java.util.List;

public class RemisionUseCase implements RemisionFactory {

   private final RemisionCrudRepository remisionCrudRepository;

    public RemisionUseCase(RemisionCrudRepository remisionRepository) {
        this.remisionCrudRepository = remisionRepository;
    }

    public Flux<Remision> consultarRemisiones(){
        return remisionCrudRepository.consultarRemisiones();
    }

    public Mono<Boolean> crearRemisionCitas (RemisionRequest remisionRequest, List<CitaRequest> citas){
        return remisionCrudRepository
                .crearRemisionCita(
                        remisionRequest,
                        citas
                );
    }
    public Mono<DatosAtencionPaciente> consultarDatosAtencionPacienteByRemision(String idRemision){
        return remisionCrudRepository.consultarDatosAtencionPacienteByIdRemision(idRemision);
    }

    public Mono<Paciente> consultarPacienteFromRemision(String idRemision){
        return remisionCrudRepository.consultarPacienteFromRemision(idRemision);
    }

    public Flux<PacienteTratamientoCita> consultarAllTratamientosToFarmacia(){
        return remisionCrudRepository.consultarAllPacienteWithMedicamentosToFarmacia();
    }
    public Mono<Void> notificarMedicamentosToFarmacia (List<PacienteTratamientoCita> tratamientoCitasList){
        return remisionCrudRepository.notificarMedicamentosToFarmacia(tratamientoCitasList);
    }
    public Flux<RegistroHistorialRemision> consultarHistorialRemisionById (String idRemision){
        return remisionCrudRepository.consultarHistoricoRemision(idRemision);
    }

    public Mono<Void> actualizarRemisionPorNovedad(
            RemisionRequest remisionRequest, List<CitaRequest> citas, NovedadRequest novedadRequest){
        return remisionCrudRepository.actualizarRemisionPorNovedad(remisionRequest,citas,novedadRequest);
    }
    public Mono<RegistroHistorialRemision> consultarDataActualRemision(String idRemision){
        return remisionCrudRepository.consultarDatosRemision(idRemision);
    }
    public Mono<Boolean> egresarRemisionById(String idRemision){
        return remisionCrudRepository.egresarRemisionById(idRemision);
    }
}
