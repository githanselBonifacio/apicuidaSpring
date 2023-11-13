package co.com.sura.moviles;

import co.com.sura.entity.moviles.MovilRepository;

public class MovilesUseCase {

    private final MovilRepository movilRepository;

    public MovilesUseCase(MovilRepository movilRepository) {
        this.movilRepository = movilRepository;
    }
}
