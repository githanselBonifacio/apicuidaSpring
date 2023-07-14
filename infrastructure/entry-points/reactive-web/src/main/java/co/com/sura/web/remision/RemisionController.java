package co.com.sura.web.remision;


import co.com.sura.dto.remision.CrearRemisionCitasRequest;
import co.com.sura.entity.remision.DatosAtencionPaciente;
import co.com.sura.entity.remision.Paciente;
import co.com.sura.remision.RemisionUseCase;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.web.bind.annotation.*;
import reactor.core.publisher.Flux;
import reactor.core.publisher.Mono;


@RestController
@RequestMapping("/remision")
public class RemisionController {

    @Autowired
    RemisionUseCase remisionUseCase;

    //remision
    @PostMapping(value = "/crearRemisionCitas")
    public Mono<Void> crearRemisioCitas(@RequestBody CrearRemisionCitasRequest crearRemisionCitasRequest) {
        return remisionUseCase.crearRemisionCitas(
                crearRemisionCitasRequest.getRemision(),
                crearRemisionCitasRequest.getCitas()
        );
    }

    @GetMapping(value = "datosAtencionPaciente/{idRemision}")
    public Mono<DatosAtencionPaciente> consultarDatosAtencionPacienteByRemision(@PathVariable String idRemision){
        return remisionUseCase.consultarDatosAtencionPacienteByRemision(idRemision);
    }

    @GetMapping(value = "pacienteFromRemision/{idRemision}")
    public Mono<Paciente> consultarPacienteFromRemision(@PathVariable String idRemision){
        return remisionUseCase.consultarPacienteFromRemision(idRemision);
    }
}
