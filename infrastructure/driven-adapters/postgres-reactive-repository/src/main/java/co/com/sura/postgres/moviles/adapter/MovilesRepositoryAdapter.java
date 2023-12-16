package co.com.sura.postgres.moviles.adapter;

import co.com.sura.moviles.gateway.MovilRepository;
import co.com.sura.postgres.moviles.data.DesplazamientoRepository;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Repository;

@Repository
public class MovilesRepositoryAdapter implements MovilRepository {

    @Autowired
    private DesplazamientoRepository desplazamientoRepository;
}